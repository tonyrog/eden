%%
%% Read fasta format
%% ';' <comment>
%% '>' <description>
%%  <chars>
%%  [<chars>]*
%%
-module(eden_fasta).

-export([load/1]).  %% load DNA sequence

-compile(export_all).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

load(File) ->
    RList = 
	fold(File,
	     fun([_ID,_Pos,Seq],Acc) ->
		     [Seq|Acc]
	     end, []),
    lists:reverse(RList).

scan(File) ->
    scan(File,".*",[]).

%% Hint: search repeating structures (ACGT){n,m}
%% find from n to m repeats of ACGT in sample data
scan(File,RE) ->
    scan(File,RE,[global],0).

scan(File,RE,REOpts) ->
    scan(File,RE,REOpts,0).
    
scan(File,RE,REOpts,N) when is_integer(N), N >= 0->
    try scan_(File,RE,REOpts,N) of
	R -> R
    catch
	throw:limit ->
	    limit
    end.

scan_(File,RE,REOpts,N) ->
    {ok,REComp} = re:compile(RE),
    fold(File,
	 fun([ID,Pos,SEQ], RecNo0) ->
		 io:format("~w: id=~s, length=~p\n", 
			   [Pos,ID,byte_size(SEQ)]),
		 RecNo = RecNo0+1,
		 case re:run(SEQ, REComp, REOpts) of
		     nomatch -> ok;
		     {match,M} -> 
			 io:format("match: ~w\n", [M])
		 end,
		 if N > 0, RecNo >= N ->
			 throw(limit);
		    RecNo band 16#fff =:= 0 ->
			 io:format("."),
			 if RecNo band 16#3ffff =:= 0 ->
				 io:format("\n");
			    true ->
				 ok
			 end;
		    true ->
			 ok
		 end,
		 RecNo
	 end, 0).
		 
fold(File, Fun, Acc) ->
    case open_read(File) of
	{ok,Fd} ->
	    try fold_(Fd, Fun, Acc) of
		R -> R
	    catch
		?EXCEPTION(error,Code,Stack) ->
		    io:format("crash: ~p\n", [Stack]),
		    {error,Code}
	    end;
	Error ->
	    Error
    end.

fold_(In, Fun, Acc) ->
    fold_(In, 1, "", [], Fun, Acc).

fold_(In, Pos, ID, Buf, Fun, Acc) ->
    case file:read_line(In) of
	eof ->
	    if Buf =:= [] ->
		    Acc;
	       true ->
		    Data = iolist_to_binary(lists:reverse(Buf)),
		    Fun([ID,Pos,Data], Acc)
	    end;
	{ok,<<";",_Comment/binary>>} ->
	    %% ignore comments
	    fold_(In,Pos,ID,Buf,Fun,Acc);
	{ok,<<">",ID1/binary>>} ->
	    NextID = string:trim(ID1),
	    if Buf =:= [] ->
		    fold_(In, Pos, NextID, Buf, Fun, Acc);
	       true ->
		    Data = iolist_to_binary(lists:reverse(Buf)),
		    Acc1 = Fun([ID,Pos,Data], Acc),
		    fold_(In,Pos+byte_size(Data),NextID,[],Fun,Acc1)
	    end;
	{ok,Bin} ->
	    fold_(In,Pos,ID,[trim_nl(Bin)|Buf],Fun,Acc)
    end.

trim_nl(Bin) ->
    Size = byte_size(Bin)-1,
    case Bin of
	<<Bin1:Size/binary,"\n">> -> Bin1;
	_ -> Bin
    end.

open_read(File) ->
    file:open(File, [raw,read,binary,read_ahead,compressed]).
