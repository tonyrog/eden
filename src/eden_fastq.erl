%%
%% Read fastq format
%% '@' <comment>
%%  <chars>
%%  [<chars>]*
%% '+'[<comment>]
%% <quality>
%% [<quality>]*
%%
-module(eden_fastq).

-compile(export_all).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

%% -define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).


scan(File) ->
    scan(File,".*",[]).

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
	 fun([_Id,Seq,_Plus,Qual], RecNo0) ->
		 RecNo = RecNo0+1,
		 ?dbg("RecNo: ~w\n", [RecNo]),
		 ?dbg(" Seqid = ~p\n", [_Id]),
		 ?dbg(" Seq = ~p\n", [Seq]),
		 ?dbg(" Qual = ~w\n", [decode_quality(Qual)]),
		 %% verify Rec
		 if byte_size(Seq) =/= byte_size(Qual) ->
			 io:format("~w, warning sequence size = ~w, quality size = ~w\n", [RecNo, byte_size(Seq), byte_size(Qual)]);
		    true ->
			 ok
		 end,
		 case re:run(Seq, REComp, REOpts) of
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
		    io:format("crash: ~p\n", [?GET_STACK(Stack)]),
		    {error,Code}
	    end;
	Error ->
	    Error
    end.

fold_(In, Fun, Acc) ->
    case file:read_line(In) of
	eof -> Acc;
	{ok,<<"@",At/binary>>} ->
	    fold1_(In, Fun, string:trim(At), [], Acc)
    end.

fold1_(In, Fun, At, CBuf, Acc) ->
    case file:read_line(In) of
	eof -> Acc; 
	{ok,<<"+",Plus/binary>>} ->
	    fold2_(In, Fun, At, string:trim(Plus), CBuf, [], Acc);
	{ok,Chars} ->
	    fold1_(In, Fun, At, [trim_nl(Chars)|CBuf], Acc)
    end.

fold2_(In, Fun, At, Plus, CBuf, QBuf, Acc) ->
    case file:read_line(In) of
	eof -> 
	    CBuf1 = iolist_to_binary(lists:reverse(CBuf)),
	    QBuf1 = iolist_to_binary(lists:reverse(QBuf)),
	    Fun([At,CBuf1,Plus,QBuf1], Acc);
	{ok,<<"@",At1/binary>>} ->
	    CBuf1 = iolist_to_binary(lists:reverse(CBuf)),
	    QBuf1 = iolist_to_binary(lists:reverse(QBuf)),
	    Acc1 = Fun([At,CBuf1,Plus,QBuf1], Acc),
	    fold1_(In, Fun, string:trim(At1), [], Acc1);
	{ok,Qual} ->
	    fold2_(In, Fun, At, Plus, CBuf, [trim_nl(Qual)|QBuf], Acc)
    end.

trim_nl(Bin) ->
    Size = byte_size(Bin)-1,
    case Bin of
	<<Bin1:Size/binary,"\n">> -> Bin1;
	_ -> Bin
    end.


decode_quality(Q) ->
    list_to_tuple([ decode_q(C, sanger) || <<C>> <= Q ]).

decode_q(C,sanger) when C >= 33, C =< 126 ->
    P = C - 33,  %% P in range 0..93
    P;
decode_q(C,illumina_10) when C >= 59, C =< 126 ->
    P = C - 64,  %% P in range -5..62
    P;
decode_q(C,illumina_13) when C >= 62, C =< 126 ->
    P = C - 62,
    P;
decode_q(C,illumnina_15) when C >= 59, C =< 126, 
				     C =/= $@, C =/= $A ->
    case C - 64 of %% P in range -5..62 (excluding 0,1)
	2 -> n;
	P -> P
    end;
decode_q(C,illumina_18) when C >= 33, C =< 126 ->
    P = C - 33,  %% P in range 0..93
    P.

phred_q(P, sanger) ->
    -10*math:log10(P);
phred_q(P, illumina_10) ->
    -10*math:log10(P/(1-P)).

open_read(File) ->
    file:open(File, [raw,read,binary,read_ahead,compressed]).
