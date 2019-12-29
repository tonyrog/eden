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

-define(CHUNK_SIZE, 1024).

scan(File) ->
    scan(File,verify,0).

scan(File,Pattern) ->
    scan(File,Pattern,0).

scan(File,verify,N) ->
    scan(File,"",N);
scan(File,all,N) ->
    scan(File,".*",N);
scan(File,Pattern,N) when is_integer(N), N >= 0->
    try scan_(File,Pattern,N) of
	R -> R
    catch
	throw:limit ->
	    limit
    end.

scan_(File,Pattern,N) ->
    {ok,RE} = re:compile(Pattern),
    fold(File,
	 fun(Rec=[ID,SEQ,_,Q], RecNo0) ->
		 RecNo = RecNo0+1,
		 %% verify Rec
		 if byte_size(SEQ) =/= byte_size(Q) ->
			 io:format("~w, warning sequence size = ~w, quality size = ~w\n", [RecNo, byte_size(SEQ), byte_size(Q)]);
		    true ->
			 ok
		 end,
		 case re:run(ID, RE) of
		     nomatch -> ok;
		     {match,_} ->
			 io:format("~w: ~p\n", [RecNo, Rec])
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

fold_(Fd, Fun, Acc) ->
    fold_(Fd, <<>>, [], Fun, Acc).

fold_(Fd, Buf, Rec, Fun, Acc) ->
    case file:read(Fd, ?CHUNK_SIZE) of
	eof ->
	    case Rec of
		[_ID,_SEQ,_PLUS,_Q] ->
		    {eof, Fun(Rec, Acc), Buf};
		[] ->
		    {eof, Acc, Buf};
		_ ->
		    io:format("warn trailing lines: ~p\n", [Rec]),
		    {eof, Acc, Buf}
	    end;
	{ok,Bin} ->
	    split_(Fd, <<Buf/binary, Bin/binary>>, Rec, Fun, Acc)
    end.

split_(Fd, Buf, Rec, Fun, Acc) ->
    case binary:split(Buf, <<$\n>>) of
	[Ln,Buf1] ->
	    case trim(Ln) of %% only check for empty lines!
		<<>> ->
		    split_(Fd, Buf1, Rec, Fun, Acc);
		_ -> %% not trimmed!
		    add_(Fd, Buf1, Ln, Rec, Fun, Acc)
	    end;
	[Buf1] ->
	    fold_(Fd, Buf1, Rec, Fun, Acc)
    end.

trim(<<$\s,Tail/binary>>) -> trim(Tail);
trim(<<$\t,Tail/binary>>) -> trim(Tail);
trim(Tail) -> Tail.

add_(Fd, Buf1, ID = <<$@,_/binary>>, [], Fun, Acc) ->
    split_(Fd, Buf1, [ID], Fun, Acc);
add_(Fd, Buf1, ID1 = <<$@,_/binary>>, Rec=[_ID,_SEQ,_PLUS,_Q], Fun, Acc) ->
    Acc1 = Fun(Rec, Acc),
    split_(Fd, Buf1, [ID1], Fun, Acc1);
add_(Fd, Buf1, SEQ, [ID], Fun, Acc) ->
    split_(Fd, Buf1, [ID,SEQ], Fun, Acc);
add_(Fd, Buf1, PLUS = <<$+,_/binary>>, [ID,SEQ], Fun, Acc) ->
    %% used?
    split_(Fd, Buf1, [ID,SEQ,PLUS], Fun, Acc);
add_(Fd, Buf1, SEQ1, [ID,SEQ], Fun, Acc) ->
    SEQ1t = trim(SEQ1),
    split_(Fd, Buf1, [ID,<<SEQ/binary,SEQ1t/binary>>], Fun, Acc);
add_(Fd, Buf1, Q, [ID,SEQ,PLUS], Fun, Acc) ->
    split_(Fd, Buf1, [ID,SEQ,PLUS,Q], Fun, Acc);
add_(Fd, Buf1, Q1, [ID,SEQ,PLUS,Q], Fun, Acc) ->
    Q1t = trim(Q1),
    split_(Fd, Buf1, [ID,SEQ,PLUS,<<Q/binary,Q1t/binary>>], Fun, Acc);
add_(Fd, Buf1, _Skip, Rec, Fun, Acc) ->
    io:format("skip ~p\n", [_Skip]),  %% restart?
    split_(Fd, Buf1, Rec, Fun, Acc).
    
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
