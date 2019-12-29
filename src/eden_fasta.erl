%%
%% Read fasta format
%% ';' <comment>
%% '>' <description>
%%  <chars>
%%  [<chars>]*
%%
-module(eden_fasta).

-compile(export_all).

scan(File) ->
    scan(File,".*",[]).

%% Hint: search repeating structures (ACGT){n,m}
%% find from n to m repeats of ACGT in sample data
scan(File,Pattern) ->
    scan(File,Pattern,[global],0).

scan(File,Pattern,Opts) ->
    scan(File,Pattern,Opts,0).
    
scan(File,Pattern,MatchOpts,N) when is_integer(N), N >= 0->
    try scan_(File,Pattern,MatchOpts,N) of
	R -> R
    catch
	throw:limit ->
	    limit
    end.

scan_(File,Pattern,MatchOpts,N) ->
    {ok,RE} = re:compile(Pattern),
    fold(File,
	 fun([ID,Pos,SEQ], RecNo0) ->
		 io:format("~w: id=~s, length=~p\n", 
			   [Pos,ID,byte_size(SEQ)]),
		 RecNo = RecNo0+1,
		 case re:run(SEQ, RE, MatchOpts) of
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
		error:Code:Stack ->
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
	    %% remove the trailing newline
	    Size = byte_size(Bin)-1,
	    case Bin of
		<<Line:Size,"\n">> ->
		    fold_(In,Pos,ID,[Line|Buf],Fun,Acc);
		_ ->
		    fold_(In,Pos,ID,[Bin|Buf],Fun,Acc)
	    end
    end.

open_read(File) ->
    file:open(File, [raw,read,binary,read_ahead,compressed]).
