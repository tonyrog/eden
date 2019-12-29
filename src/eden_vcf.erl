%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Scan VCF file 
%%% @end
%%% Created : 25 Dec 2019 by Tony Rogvall <tony@rogvall.se>

-module(eden_vcf).

-compile(export_all).
-export([count_chrom_snp/1]).
-export([extract_chrom/3]).
-export([read_file_header/1]).
-export([display_file_header/1]).
-export([display_header/1]).

%% Extract Chromosome "1".."22","X","Y","MT" ..
%% DstFile must not exist!
extract_chrom(SrcFile, CHROM, DstFile) ->
    case filelib:is_file(DstFile) of
	true ->
	    io:format("error: Destination file \"~s\" already exist\n",
		      [DstFile]),
	    error;
	false ->
	    case open_write(DstFile) of
		{ok,Out} ->
		    case open_read(SrcFile) of
			{ok,In} ->
			    try read_header_(In, #{}, Out) of
				_Hdr ->
				    Result = scan_chrom_(In, CHROM, Out),
				    close(In),
				    close(Out),
				    Result
			    catch
				error:Reason ->
				    close(In),
				    close(Out),
				    {error,Reason}
			    end;
			Error ->
			    close(Out),
			    Error
		    end;
		Error ->
		    Error
	    end
    end.

scan_chrom_(In, CHROM, Out) ->
    eden_csv:foldfd(In,[{fc,$\t},{comment,$#}],
		    fun(Line=[CHR,_POS,_ID,_REF,_ALT,_QUAL,_FILTER,_INFO | _],
			_St) ->
			    if CHR =:= CHROM ->
				    write_data(Out, [lists:join("\t", Line),
						     "\n"]);
			       true ->
				    ok
			    end
		    end, ok).

read_file_header(File) ->
    case open_read(File) of 
	{ok,Fd} ->
	    try read_header_(Fd, #{}, undefined) of
		Header -> Header
	    after
		close(Fd)
	    end;
	Error ->
	    Error
    end.

display_file_header(File) ->
    case read_file_header(File) of
	Error = {error, _} -> Error;
	Hdr -> display_header(Hdr)
    end.

display_header(Hdr) ->
    io:format("version: ~s\n", [maps:get(fileformat,Hdr,"undefined")]),
    io:format("reference: ~s\n", [maps:get(reference,Hdr,"undefined")]),
    io:format("fileoffset: ~w\n", [maps:get(fileoffset,Hdr,0)]),
    display_alt(Hdr),
    display_info(Hdr),
    display_format(Hdr),
    display_filter(Hdr),
    Total = display_chrom_lengths(Hdr),
    io:format("total length = ~w\n", [Total]).

%% convert lengths into file offsets
display_chrom_lengths(Hdr) ->
    CHROMList = lists:reverse(maps:get(contig,Hdr,[])),
    lists:foldl(
      fun(CHROM, TotalLength) ->
	      Attr = maps:get({contig,CHROM},Hdr,[]),
	      Length = proplists:get_value("length", Attr, 0),
	      if is_integer(CHROM) ->
		      io:format("Chr~w: length: ~w\n", [CHROM, Length]);
		 CHROM =:= "X"; CHROM =:= "Y" ->
		      io:format("Chr~s: length: ~w\n", [CHROM, Length]);
		 true ->
		      io:format("~s: length: ~w\n", [CHROM, Length])
	      end,
	      TotalLength + Length
      end, 0, CHROMList).

%% ALT Fields
%%  
display_alt(Hdr) -> display_key(Hdr, alt).
%% INFO Fields
%%   Type = Integer|Float|Character|String|Flag
%%   Number = 1, 2, .. N | A | R | G | .
%%      A = one value per alternate allele
%%      R = 
%%      G = 
%%      . =  varies
%%   Flag = 
%% Tag names must match "^[A-Za-z][0-9A-Za-z.]*$"

display_info(Hdr) -> display_key(Hdr, info).
%% FORMAT Fields
%% Type = Integer|Float|Character|String
%% Tag names must match "^[A-Za-z][0-9A-Za-z.]*$"
display_format(Hdr) -> display_key(Hdr, format).
display_filter(Hdr) -> display_key(Hdr, filter).

display_key(Hdr, Key) ->
    lists:foreach(
      fun(ID) ->
	      Attr = maps:get({Key,ID}, Hdr, []),
	      io:format("~s ~s: ~p\n", [Key, ID, Attr])
      end, maps:get(Key, Hdr, [])).

read_header_(In, H, Out) ->
    case file:read_line(In) of
	eof -> H;
	{ok,Line = <<"##",HeaderLine/binary>>} ->
	    ok = write_data(Out, Line),
	    case HeaderLine of
		<<"fileformat=",Fmt/binary>> ->
		    read_header_(In,H#{ fileformat => string:trim(Fmt) },Out);
		<<"reference=", Field/binary>> ->
		    read_header_(In,H#{ reference => string:trim(Field) },Out);
		<<"contig=", Fields/binary>> ->
		    parse_header_(contig, Fields, In, H, Out);
		<<"ALT=", Fields/binary>> ->
		    parse_header_(alt, Fields, In, H, Out);
		<<"INFO=", Fields/binary>> ->
		    parse_header_(info, Fields, In, H, Out);
		<<"FILTER=", Fields/binary>> ->
		    parse_header_(filter, Fields, In, H, Out);
		<<"FORMAT=", Fields/binary>> ->
		    parse_header_(format, Fields, In, H, Out);
		_ ->
		    case binary:split(HeaderLine, <<"=">>) of
			[Key, Fields] ->
			    parse_header_(Key, Fields, In, H, Out);
			_ ->
			    io:format("Ignore header: ~p\n", [HeaderLine]),
			    read_header_(In, H, Out)
		    end
	    end;
	{ok,Line = <<"#CHROM",_HeaderLine/binary>>} ->
	    ok = write_data(Out, Line),
	    {ok,Offset} = file:position(In, {cur, 0}),
	    H#{ fileoffset => Offset };
	{ok,Line} ->
	    ok = write_data(Out, Line),
	    io:format("Got line: ~p\n", [Line]),
	    {ok,Offset} = file:position(In, {cur, 0}),
	    H#{ fileoffset => Offset }
    end.

parse_header_(Name, Fields, In, H, Out) ->
    case parse_fields(Fields) of
	error ->
	    read_header_(In, H, Out);
	As ->
	    case lists:keytake("ID", 1, As) of
		{value,{"ID",ID},As1} ->
		    IDs = maps:get(Name, H, []),
		    read_header_(In, H#{ Name => [ID|IDs],
					 {Name,ID} => As1}, Out);
		false ->
		    read_header_(In, H#{ Name => As }, Out)
	    end
    end.

parse_fields(Fields) ->
    %% FIXME use other scanner !!!
    case scan(binary_to_list(Fields)) of
	[{char,$<} | Flds] ->
	    parse_flds(Flds,[]);
	_ ->
	    error
    end.

parse_flds([{word,Name},{char,$=},{integer,Value}|Flds],Acc) ->
    parse_next_flds(Flds, [{Name,Value}|Acc]);
parse_flds([{word,Name},{char,$=},{string,Value}|Flds],Acc) ->
    parse_next_flds(Flds, [{Name,Value}|Acc]);
parse_flds([{word,Name},{char,$=},{word,Value}|Flds],Acc) ->
    parse_next_flds(Flds, [{Name,Value}|Acc]);
parse_flds(_, _Acc) ->
    error.

parse_next_flds([{char,$,}|Flds], Acc) ->
    parse_flds(Flds, Acc);
parse_next_flds([{char,$>}], Acc) ->
    lists:reverse(Acc);
parse_next_flds(_, _Acc) ->
    error.

scan([C|Cs]) when C >= $0, C =< $9 -> scan_int(Cs, C-$0);
scan([$"|Cs]) -> scan_string(Cs, []);
scan([C|Cs]) when C >= $a, C =< $z -> scan_word(Cs, [C]);
scan([C|Cs]) when C >= $A, C =< $Z -> scan_word(Cs, [C]);
scan([$\s|Cs]) -> scan(Cs);
scan([$\t|Cs]) -> scan(Cs);
scan([$\n|Cs]) -> scan(Cs);
scan([C|Cs]) -> [{char,C}|scan(Cs)];
scan([]) -> [].

scan_int([C|Cs], Int) when C >= $0, C =< $9 ->
    scan_int(Cs, Int*10+(C-$0));
scan_int(Cs, Int) ->
    [{integer,Int}|scan(Cs)].

scan_word([C|Cs], Word) when C >= $0, C =< $9 ->
    scan_word(Cs, [C|Word]);
scan_word([C|Cs], Word) when C >= $a, C =< $z ->
    scan_word(Cs, [C|Word]);
scan_word([C|Cs], Word) when C >= $A, C =< $Z ->
    scan_word(Cs, [C|Word]);
scan_word([C|Cs], Word) when C =:= $.; C =:= $_ ; C =:= $- ->
    scan_word(Cs, [C|Word]);
scan_word(Cs, Word) ->
    [{word,lists:reverse(Word)}|scan(Cs)].

scan_string([$\\,C|Cs], Str) ->
    scan_string(Cs, [C|Str]);
scan_string([$"|Cs], Str) ->
    [{string,lists:reverse(Str)} | scan(Cs)];
scan_string([C|Cs], Str) ->
    scan_string(Cs, [C|Str]);
scan_string([], Str) ->
    [{string,lists:reverse(Str)}].

count_chrom_snp(File) ->
    case open_read(File) of 
	{ok,Fd} ->
	    Hdr = read_header_(Fd, #{}, undefined),
	    Tab = ets:new(chr, []),
	    chr_snp_init(Tab),
	    chr_count_snp_(Fd, Tab, Hdr),
	    emit_count(Tab),
	    close(Fd);
	Error ->
	    Error
    end.

chr_count_snp_(Fd,Tab,_Hdr) ->
    csv:foldfd(Fd,[{fc,$\t},{comment,$#}],
	       fun([CHR,_POS,_ID,REF,ALT,_QUAL,_FILTER,INFO | _],_St) ->
		       INFOList = string_to_proplist(INFO, ";", "="),
		       AC = proplists:get_value("AC",INFOList),
		       SNP = snp(REF,ALT,AC),
		       (catch ets:update_counter(Tab, {CHR,SNP}, 1))
%%		       catch
%%			   error:_ ->
%%			       io:format("miss: REF=~w ALT=~w AC=~w\n", 
%%					 [ALT,REF,AC])
%%		       end
	       end, ok).

chr_list() ->
    [integer_to_list(I) || I <- lists:seq(1,22)] ++ ["X", "Y", "MT"].

snp_list() ->
    ["AA","CC","GG","TT"]++[[A,B] || A <- "ACGT", B <- "ACGT", A<B].    

chr_snp_init(Tab) ->
    CHRList = chr_list(),
    SNPList = snp_list(),
    [ets:insert(Tab,{{CHR,SNP},0}) || CHR <- CHRList, SNP <- SNPList]. 

%%
%% from http://www.beholdgenealogy.com/blog/?p=2879
%%
%% 1. If a line in the VCF file has one REF value and one ALT value, then
%%    * If the INFO field contains:  “AC=1”, then you take the two of them. 
%%       e.g. REF=T, ALT=C, then value is TC (or CT if you sort alphabetically)
%%    * If the INFO field contains:  “AC=2”, then you use the ALT value twice.
%%       e.g.  REF=T, ALT=C, then value is CC.
%%
%% 2. If a line in the VCF file has one REF and two ALT values, then you take
%%     both the ALT values.  e.g.  REF=T, ALT=C,G, then value is CG.  There 
%%     are only a few hundred of these in my VCF file.
%%
%% 3. If a SNP that they use is not in the VCF file, then use the reference. 
%%    e.g. REF=C, to give the value CC. They’ll need to have a reference table
%%    with the Build 37 genome reference values for all the SNPs that they use.
%%    This table would be the same for everyone.
%% 

%% combine REF and ALT into a SNP pair
snp(_REf,[X],"2") -> [X,X];
snp(_Ref,[X,$,,Y],"1") -> sort(X,Y);
snp([X],[Y,$,,_Z],"1,1") -> sort(X,Y);  %% X replaced by Y,Z (insert)
snp([X],[Y],"1") -> sort(X,Y);          %% X replaced by Y
snp([X],[Y],undefined) -> sort(X,Y).    %% X replaced by Y

sort(A,B) when A < B -> [A,B];
sort(A,B) -> [B,A].


edit_vcf(REF, ALT) ->
    case string:split(ALT, ",") of
	[[R]] ->
	    case REF of
		[_] -> [R];        %% single replace
		[R|As] -> [R|As]   %% insert ont or more
	    end;
	[[R|Ds]] ->
	    case REF of
		[R] -> [R|lists:duplicate(length(Ds),$-)] %% deletion
	    end;
	[ [R|_Rs], [R,R|As] ] ->
	    case REF of
		[R|_Ds] -> [R|As]
	    end
    end.


emit_count(Tab) ->
    CHRList = chr_list(),
    SNPList = snp_list(),
    %% write header
    io:format("~8s|", [""]),
    lists:foreach(fun(SNP) -> io:format("~6s|", [SNP]) end, SNPList),
    io:format("\n"),
    %% write elements
    lists:foreach(
      fun(CHR) ->
	      io:format("~8s|", ["Chr"++CHR]),
	      lists:foreach(
		fun(SNP) ->
			Count = case ets:lookup(Tab, {CHR,SNP}) of
				    [] -> 0;
				    [{_,N}] -> N
				end,
			io:format("~6w|", [Count])
		end, SNPList),
	      io:format("\n")
      end, CHRList).


string_to_proplist(String, ItemSeparator, KeySeparator) ->
    [case string:split(T,KeySeparator) of
	 [K,V] -> {K,V};
	 K -> {K,true}
     end || T <- string:tokens(String, ItemSeparator)].

open_read(File) ->
    file:open(File, [raw,read,binary,read_ahead,compressed]).

open_write(File) ->
    file:open(File, [raw,write,delayed_write,compressed]).

write_data(undefined, _) ->
    ok;
write_data(Fd, Data) ->
    file:write(Fd, Data).

close(Fd) ->
    file:close(Fd).
