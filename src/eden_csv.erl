%%% File    : eden_csv.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : cvs comma separated items
%%% Created : 30 Jul 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(eden_csv).

-export([read/3, write/4]).
-export([fold/3, fold/4]).
-export([foldfd/4]).
-export([line/1, line/2]).
-export([options/1, options/2]).

-import(lists, [reverse/1]).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-record(opt,
	{
	  line = 1,	  
	  file = "",        %% file name if used
	  comment,          %% comment character
	  fc = $;,          %% field delimiter
	  quote,            %% quote character
	  xquote,           %% extra quote character
	  qnl = false,      %% allow new line in quotes
	  nl = $\n,         %% new line char
	  space = trim,     %% field white space handling 
	  chunk_size = 1024 %% Block size to chunk read
	}).

-define(char(X), (((X) >= 0) and ((X) =< 255))).

%% create a options record
options(Opts) ->
    options(Opts, #opt {}).

options([{Key,Val}|Options],Opt) ->
    case Key of
	line when is_integer(Val) ->
	    options(Options, Opt#opt { line = Val });
	comment when ?char(Val) ->
	    options(Options, Opt#opt { comment = Val });
	fc when ?char(Val) ->
	    options(Options, Opt#opt { fc = Val });
	quote when ?char(Val) ->
	    options(Options, Opt#opt { quote = Val });
	xquote when ?char(Val) ->
	    options(Options, Opt#opt { xquote = Val });
	qnl when Val =:= true ->
	    options(Options, Opt#opt { qnl = true });
	qnl when Val =:= false ->
	    options(Options, Opt#opt { qnl = false });
	nl when ?char(Val) ->
	    options(Options, Opt#opt { nl = Val });
	space when Val =:= keep ->
	    options(Options, Opt#opt { space = keep });
	space when Val =:= trim ->
	    options(Options, Opt#opt { space = trim });
	space when Val =:= normalize ->
	    options(Options, Opt#opt { space = normalize });
	chunk_size when is_integer(Val), Val>0 ->
	    options(Options, Opt#opt { chunk_size = Val });
	_ ->
	    erlang:error(badarg)
    end;
options([],Opt) ->
    Opt.

%%
%% Read a csv file filtered by Fun
%%
read(Fun, File, Opt) when is_record(Opt, opt) ->
    read_(Fun, File, Opt);
read(Fun, File, Opts) when is_list(Opts) ->
    read_(Fun, File, options(Opts)).

read_(Fun, File, Opt) ->
    Res = fold1(File, Opt, 
		fun(Ts, {I,Acc}) ->
			case Fun(I,Ts) of
			    false -> {I+1,Acc};
			    true  -> {I+1,[Ts|Acc]};
			    {true,Ts1} -> {I+1,[Ts1|Acc]}
			end
		end, {1,[]}),
    case Res of
	{error,_Reason} -> 
	    Res;
	{_Line,Rs} when is_list(Rs) ->
	    {ok, list_to_tuple(lists:reverse(Rs))}
    end.

%%
%% Write a table as a cvs file where table is 
%% either a tuple/list of tuple/list
%%
write(Fun, Table, File, Opt) when is_function(Fun,2),
				  is_record(Opt, opt) ->
    write_(Fun, Table, File, Opt);
write(Fun, Table, File, Options) when is_function(Fun,2),
				      is_list(Options) ->
    write_(Fun, Table, File, options(Options)).

write_(Fun, Table, File, Opt) ->
    case write_open(File) of
	{ok,Fd} ->
	    Q = if Opt#opt.quote =:= undefined -> "";
		   true -> [Opt#opt.quote]
		end,
	    Qs = [Opt#opt.comment,Opt#opt.fc,Opt#opt.quote,Opt#opt.xquote,
		  $\s,$\t,$\n,$\r],
	    try write__(Fun, 1, Table, Fd, Q, Qs, Opt) of
		Res -> Res
	    catch
		error:Reason -> {error,Reason}
	    after
		write_close(Fd)
	    end;
	Error ->
	    Error
    end.

write_open(user) -> {ok, user};
write_open(File) -> file:open(File, [raw,write]).

write_close(user) -> ok;
write_close(Fd) -> file:close(Fd).
    

write__(_Fun, _I,  [], _Fd, _Q, _Qs, _Opt) ->
    ok;
write__(_Fun, I,  Table, _Fd, _Q, _Qs, _Opt) when I > tuple_size(Table) ->
    ok;
write__(Fun, I, Table, Fd, Q, Qs, Opt) ->    
    if is_list(Table) ->
	    write_row_(Fun,I,hd(Table),Fd,Q,Qs,Opt),
	    write__(Fun,I+1,tl(Table),Fd,Q,Qs,Opt);
       is_tuple(Table) ->
	    write_row_(Fun,I,element(I,Table),Fd,Q,Qs,Opt),
	    write__(Fun,I+1,Table,Fd,Q,Qs,Opt)
    end.

write_row_(Fun,I,Row, Fd,Q,Qs,Opt) ->
    case Fun(I, Row) of
	{true, Row1} -> write_row__(Row1,Fd,Q,Qs,Opt);
	true -> write_row__(Row,Fd,Q,Qs,Opt);
	false -> ok
    end.

write_row__(Row,Fd,Q,Qs,Opt) ->
    Es = if is_tuple(Row) -> tuple_to_list(Row);
	    is_list(Row) -> Row
	 end,
    Es1 = [case need_quote(E,Qs) of
	       true -> [Q,E,Q];
	       false -> E
	   end || E <- Es],
    file:write(Fd, [string:join(Es1, [Opt#opt.fc]), Opt#opt.nl]).

%% check if E need quotes,
%% E contain fc, comment character, quote, xquote, nl, space
need_quote(Cs, Specials) ->
    lists:any(fun(C) -> lists:member(C, Specials) end, Cs).

%%
%% Load a XLS/CSV (:;,... separated) 
%%
fold(File,Fun,St) ->
    fold1(File,#opt { },Fun,St).

fold(File,Opts,Fun,St) when is_list(Opts) ->
    fold1(File, options(Opts), Fun, St);
fold(File,Opt,Fun,St) when is_record(Opt, opt) ->
    fold1(File, Opt, Fun, St).

fold1(File, Opt, Fun, St) ->
    case file:open(File, [raw,read,binary,read_ahead,compressed]) of
	{ok,Fd} ->
	    try foldfd_(Fd,[],Opt#opt { file = File}, Fun, 
		       Opt#opt.line, St) of
		Res -> Res
	    catch
		?EXCEPTION(error,Reason,Stacktrace) ->
		    error({Reason, ?GET_STACK(Stacktrace)})
	    after
		file:close(Fd)
	    end;
	Error -> Error
    end.

foldfd(Fd, Opts, Fun, St) when is_list(Opts) ->
    foldfd(Fd, options(Opts), Fun, St);
foldfd(Fd, Opt, Fun, St) when is_record(Opt, opt) ->
    foldfd_(Fd, [], Opt, Fun, Opt#opt.line, St).

foldfd_(Fd, Cs, Opt, Fun, Ln, St) ->
    case scan_tokens(Fd,Cs,Ln,[],Opt) of
	eof ->
	    St;
	{[],Cs1,Ln1} ->
	    foldfd_(Fd, Cs1, Opt, Fun, Ln1, St);
	{Ts,Cs1,Ln1} ->
	    case is_empty(Ts) of
		true ->
		    foldfd_(Fd,Cs1, Opt, Fun, Ln1, St);
		false ->
		    St1 = Fun(Ts, St),
		    foldfd_(Fd,Cs1,Opt,Fun,Ln1,St1)
	    end
    end.

line(Data) ->
    line1(Data, #opt {}).

line(Data, Opts) when is_list(Opts) ->
    line1(Data, options(Opts));
line(Data, Opt) when is_record(Opt, opt) ->
    line1(Data, Opt).

line1(LineData, Opt) ->
    case scan_tokens(string,LineData,Opt#opt.line,[],Opt) of
	eof ->
	    [];
	{Ts,_,_} -> Ts
    end.
    

scan_tokens(Fd,Cs,Ln,Ts,Opt) ->
    scan_tokens(Fd,Cs, Ln, [], Ts, Opt).

scan_tokens(Fd,[C|Cs],Ln,Acc,Ts,Opt) ->
    if C =:= Opt#opt.fc ->
	    scan_tokens(Fd,Cs,Ln,[token(Acc,Opt#opt.space)|Ts], Opt);
       C =:= Opt#opt.nl ->
	    {reverse([token(Acc,Opt#opt.space)|Ts]),Cs,Ln+1};
       C =:= Opt#opt.comment ->
	    Cs1 = skip_line(Fd, Cs, Opt),
	    {reverse([token(Acc,Opt#opt.space)|Ts]),Cs1,Ln};
       C =:= Opt#opt.quote ->
	    scan_quote(Fd,Cs,Ln,C,Acc,Ts,Opt);
       C =:= Opt#opt.xquote ->
	    scan_quote(Fd,Cs,Ln,C,Acc,Ts,Opt);
       true ->
	    scan_tokens(Fd,Cs,Ln,[C|Acc],Ts,Opt)
    end;
scan_tokens(Fd,[],Ln,Acc,Ts,Opt) ->
    case more(Fd, Opt#opt.chunk_size) of
	eof ->
	    if Acc =:= [], Ts =:= [] ->
		    eof;
	       true ->
		    {reverse([token(Acc,Opt#opt.space)|Ts]),[],Ln}
	    end;
	Cs ->
	    scan_tokens(Fd,Cs,Ln,Acc,Ts,Opt)
    end.


scan_quote(Fd,[C|Cs],Ln,Q,Acc,Ts,Opt) ->
    if C =:= Q ->
	    scan_tokens(Fd,Cs,Ln,Acc,Ts,Opt);
       C =:= Opt#opt.nl ->
	    if Opt#opt.qnl =:= true ->
		    scan_quote(Fd,Cs,Ln+1,Q,[C|Acc],Ts,Opt);
	       true ->
		    {reverse([token(Acc,Opt#opt.space)|Ts]),Cs,Ln}
	    end;
       true ->
	    scan_quote(Fd,Cs,Ln,Q,[C|Acc],Ts,Opt)
    end;
scan_quote(Fd,[],Ln,Q,Acc,Ts,Opt) ->
    case more(Fd,Opt#opt.chunk_size) of
	eof ->
	    if Acc =:= [], Ts =:= [] ->
		    eof;
	       true ->
		    {reverse([token(Acc,Opt#opt.space)|Ts]),[],Ln}
	    end;
	Data ->
	    scan_quote(Fd,Data,Ln,Q,Acc,Ts,Opt)
    end.

skip_line(Fd, [C|Cs], Opt) ->
    if C =:= Opt#opt.nl -> Cs;
       true -> skip_line(Fd, Cs, Opt)
    end;
skip_line(Fd, [], Opt) -> 
    case more(Fd,Opt#opt.chunk_size) of
	eof -> [];
	Cs ->  skip_line(Fd, Cs,Opt)
    end.
	
more(string,_) ->
    eof;
more(Fd,ChunkSize) ->
    case file:read(Fd, ChunkSize) of
	{ok,Data} ->
	    binary_to_list(Data);
	eof -> 
	    eof
    end.

is_empty([""|Ts]) -> is_empty(Ts);
is_empty([_|_]) -> false;
is_empty([]) -> true.

token(Rev,keep) -> %% keep blanks
    reverse(Rev);
token(Rev,trim) -> %% trim of blanks
    trim(reverse(trim(Rev)));
token([],normalize) -> %% trim blanks but leave a blank if none empty
    [];
token(Rev,normalize) -> 
    case reverse(trim(Rev)) of
	[] -> " ";
	Tok -> trim(Tok)
    end.

trim([$\s|Cs]) -> trim(Cs);
trim([$\t|Cs]) -> trim(Cs);
trim([$\n|Cs]) -> trim(Cs);
trim([$\r|Cs]) -> trim(Cs);
trim(Cs) -> Cs.
