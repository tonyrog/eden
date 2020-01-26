%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Simple drawing of DNA data
%%% @end
%%% Created : 30 Dec 2019 by Tony Rogvall <tony@rogvall.se>

-module(eden_edit).

%% API
-export([start/0, start/1]).
-export([file/1]).
-export([set_dna/1]).
-export([demo/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
-compile(export_all).

-define(SERVER, ?MODULE).

-define(BASE_WIDTH,  16).
-define(BASE_HEIGHT, 32).
-define(SEL_HEIGHT,  4).

-define(TOP_OFFSET,    8).
-define(BOTTOM_OFFSET, 4).
-define(LEFT_OFFSET,   32).
-define(RIGHT_OFFSET,  4).

-define(MIN_ZOOM, -10).
-define(MAX_ZOOM,  10).
-define(MIN_OFFSET, -100).

-define(WIDTH,  800).
-define(HEIGHT, 480).

-define(USE_OFF_SCREEN, true).
-define(USE_EXPOSURE, false).

-define(WHITE, grey5).
-define(BLACK, grey10).

-define(TEXT_COLOR,              {0,0,0,0}).       %% black text

-include_lib("epx/include/epx_menu.hrl").
-include_lib("epx/include/epx_window_content.hrl").

%% color profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 zoom                          = 0,    %% 100%
	 screen_color                  = grey2,
	 selection_alpha               = 100,
	 selection_color               = grey,
	 selection_border_width        = 1,
	 selection_border_color        = ?BLACK,
	 base_width                    = ?BASE_WIDTH,
	 base_height                   = ?BASE_HEIGHT,
	 border_width                  = 1,
	 border_color                  = ?BLACK,
	 select_color                  = ?WHITE,
	 select_border_width           = 2,
	 select_border_color           = ?BLACK,
	 highlight_color               = grey6,
	 highlight_border_width        = 2,
	 highlight_border_color        = red,
	 label_font                    = "Arial",
	 label_font_size               = 12,
	 label_font_color              = ?BLACK,
	 label_background_color        = ?BLACK,
	 label_border_color            = yellow,
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = ?WHITE,
	 menu_background_color         = ?BLACK,
	 menu_border_color             = green,

	 window_font_name              = "Arial",
	 window_font_size              = 12,
	 window_font_color             = ?BLACK,
	 scroll_bar_color              = grey6,
	 scroll_hndl_color             = gray8,
	 scroll_horizontal             = right,
	 scroll_vertical               = bottom,
	 top_bar_color                 = red,
	 left_bar_color                = green,
	 right_bar_color               = blue,
	 bottom_bar_color              = ?WHITE
	}).

-record(fi,
	{
	 font,
	 size,
	 width,
	 height,
	 ascent 
	}).

-record(state,
	{
	 backend,
	 window,
	 screen,          %% on-screen pixels
	 pixels,          %% off-screen pixels
	 grid,            %% grid pixels
	 width,           %% width of screen and pixels
	 height,          %% height of screen and pixels
	 gwidth,          %% width of grid pixels
	 gheight,         %% height of grid pixels
	 profile,         %% color profile
	 pt,              %% last button press position
	 pt1,
	 pt2,
	 operation = none :: none | menu | select | move | vertex | edge,
	 selected = [],   %% list of selected bases
	 keymod = #keymod{}, %% modifiers
	 esc = false,        %% escape key
	 dna,             %% the DNA data
	 zoom = 0,        %% zoom factor -10 ... 10
	 clip,            %% the cut/copy DNA
	 menu,            %% global menu state
	 winfo,
	 view = base :: base | short | amino | compressed,
	 color_map,       %% tuple that maps color 0..15 to RGB 
	 font,
	 font_list = [] :: [#fi{}],
	 font_used = undefined :: #fi{},
	 fcolor,
	 pos      = 0,       %% upper left corner pos of DNA
	 drows    = 0,       %% number of rows to draw (0 = all)
	 ddir     = 0,       %% direction to update rows
	 dpos     = 0,       %% draw pos rem nrows
	 nrows    = 0,
	 ncolumns = 0,
	 noffs    = 0        %% ?MIN_OFFSET..0 update on ncolumns >= 1!
	}).


-define(SHIFT(State), (State#state.keymod)#keymod.shift).
-define(CTRL(State), (State#state.keymod)#keymod.ctrl).
-define(ALT(State), (State#state.keymod)#keymod.alt).

demo() ->
    file(filename:join(code:priv_dir(eden),"mt.fa.gz")).

%%%===================================================================
%%% API
%%%===================================================================
file([Filename]) when is_atom(Filename) ->
    file0(atom_to_list(Filename));
file(Filename)  when is_list(Filename), is_integer(hd(Filename)) ->
    file0(Filename).

file0(Filename) when is_list(Filename), is_integer(hd(Filename)) ->
    case load_alt([{".fa.gz",{eden_fasta,load,[]}},
		   {".fasta.gz",{eden_fasta,load,[]}},
		   {".fa",{eden_fasta,load,[]}},
		   {".fasta",{eden_fasta,load,[]}},
		   
		   {".fq.gz",{eden_fastq,load,[]}},
		   {".fastq.gz",{eden_fastq,load,[]}},
		   {".fq",{eden_fastq,load,[]}},
		   {".fastq",{eden_fastq,load,[]}}
		  ], Filename) of
	{ok,DNA} ->
	    case start() of
		{ok,Pid} ->
		    set_dna(Pid, DNA);
		{error,{already_started,Pid}} ->
		    set_dna(Pid, DNA);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

set_dna(DNA) -> set_dna(?SERVER, DNA).
set_dna(Pid, DNA) -> gen_server:call(Pid, {set_dna, DNA}).

load_alt([{Ext, {Mod,Fun,Args}}|Alt], Filename) ->
    case lists:suffix(Ext, Filename) of
	true ->
	    {ok, apply(Mod,Fun,[Filename|Args])};
	false ->
	    load_alt(Alt, Filename)
    end;
load_alt([], _Filename) ->
    {error, bad_suffix}.
    
start() ->
    start([false]).

start([TTYLogger|Opts0]) ->
    %% (catch error_logger:tty(TTYLogger)),
    application:start(lager),
    application:load(epx),
    application:set_env(epx, use_off_screen, ?USE_OFF_SCREEN),
    application:set_env(epx, use_exposure, ?USE_EXPOSURE),
    %% epx:debug(debug),
    application:load(eden),
    Width  = application:get_env(eden, screen_width, ?WIDTH),
    Height = application:get_env(eden, screen_height, ?HEIGHT),
    application:ensure_all_started(epx),
    Opts = [{screen_width,Width},{screen_height,Height}|Opts0],
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init(Options) ->
    process_flag(trap_exit, true),
    %% options list override env
    Env = Options ++ application:get_all_env(eden),
    Width  = proplists:get_value(screen_width, Env, ?WIDTH),
    Height = proplists:get_value(screen_height, Env, ?HEIGHT),
    Profile = load_profile(Env),
    MenuProfile = create_menu_profile(Profile),
    Backend = epx_backend:default(),
    FontList =
	[begin
	     {ok,F} = epx_font:match([{name,Profile#profile.label_font},
				      {weight, medium},
				      {slant, roman},
				      {size,Size}]),
	     epx_gc:set_font(F),
	     {W,H}  = epx_font:dimension(F,"G"),
	     A = epx:font_info(F, ascent),
	     #fi{font=F,size=Size,width=W,height=H,ascent=A}
	 end || Size <- [10,12,14,16,18,
			 20,22,24,26,28,
			 30,32,34,36,38,
			 40,42,44,46,48]],
    Zoom = Profile#profile.zoom,
    Zf = zoom_factor(Zoom),
    [_,Fi0|_] = FontList,  %% pick out zoom=0 
    FWidth = trunc(?BASE_WIDTH*Zf),  %% size we need to draw
    Fi = find_base_font_(FWidth, FontList, undefined),

    WInfo = #window_info {
	       glyph_width  = Fi0#fi.width,
	       glyph_height = Fi0#fi.height,
	       glyph_ascent = Fi0#fi.ascent,
	       glyph_descent = epx:font_info(Fi0#fi.font, descent),
	       bottom_bar = 18
	      },
    Events = 
	[key_press,key_release,
	 wheel, left, right, %% motion-left-button
	 configure,button_press,button_release] ++
	case ?USE_EXPOSURE of
	    true -> [expose];
	    _ -> []
	end,
    Window = epx:window_create(40, 40, Width, Height, Events),

    epx:window_attach(Window, Backend),
    epx:window_adjust(Window, [{name, "EDEN"}]),

    NRows = nrows(WInfo, Height, Zf),
    NColumns = ncolumns(WInfo, Width, Zf),
    GWidth = trunc(NColumns*Zf*?BASE_WIDTH),
    GHeight = trunc(NRows*Zf*?BASE_HEIGHT),    
    Grid = epx:pixmap_create(GWidth, GHeight, argb),
    Pixels = epx:pixmap_create(Width, Height, argb),
    Screen = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_attach(Screen, Backend),

    Menu = epx_menu:create(MenuProfile, menu(global)),

    Scheme = Profile#profile.scheme,
    Z   = epx_profile:color(Scheme,grey),
    T   = epx_profile:color(Scheme,red),
    G   = epx_profile:color(Scheme,yellow),
    C   = epx_profile:color(Scheme,blue),
    A   = epx_profile:color(Scheme,green),

    FontColor = load_color(Scheme, label_font_color, Env, Profile),

    ColorMap = erlang:make_tuple(256,Z,
				 [{$A,color32(A)},
				  {$C,color32(C)},
				  {$G,color32(G)},
				  {$T,color32(T)},
				  {$W,color32(add_color(A,T))},
				  {$S,color32(add_color(C,G))},
				  {$M,color32(add_color(A,C))},
				  {$K,color32(add_color(G,T))},
				  {$R,color32(add_color(A,G))},
				  {$Y,color32(add_color(C,T))},
				  {$B,color32(add_color(C,G,T))},
				  {$D,color32(add_color(A,G,T))},
				  {$H,color32(add_color(A,C,T))},
				  {$V,color32(add_color(A,C,G))},
				  {$N,color32(add_color(A,C,G,T))}]),

    State = #state{ backend = Backend,
		    window  = Window,
		    screen  = Screen,   %% screen pixels
		    pixels  = Pixels,   %% off-screen pixels
		    grid    = Grid,
		    profile = Profile,
		    width   = Width,
		    height  = Height,
		    gwidth   = GWidth,
		    gheight  = GHeight,
		    menu    = Menu,
		    dna     = <<>>,
		    pos     = 0,
		    drows   = NRows,
		    ddir    = 1,
		    winfo   = WInfo,
		    color_map = ColorMap,
		    font = Fi0#fi.font,
		    font_list = FontList,
		    font_used = Fi,
		    fcolor = FontColor,
		    zoom = Zoom,
		    nrows = NRows,
		    ncolumns = NColumns
		  },
    invalidate(State),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.

handle_call({set_dna,DNAStrand}, _From, State) ->
    invalidate(State),
    DNA = iolist_to_binary(DNAStrand),
    {reply, ok, State#state { pos  = 0,
			      dna = DNA,
			      clip = <<>>,
			      selected = []}};
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({epx_event,W,Event}, State) when State#state.window =:= W ->
    %% io:format("Epx event ~p\n", [Event]),
    handle_epx_event(Event, State);
handle_info(Event, State)  ->
    io:format("Got event ~p\n", [Event]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_epx_event(Event, State) ->
    case Event of
	{button_press, [left], _Where={Xm,Ym,_Zm}} ->
	    XY = {Xm,Ym},
	    if 
		State#state.operation =:= menu ->
		    case epx_menu:find_row(State#state.menu, 
					   State#state.pt1,
					   XY) of
			{-1, _Menu} ->
			    {noreply, State};
			{_Row, Menu} ->
			    case epx_menu:command(Menu) of
				none ->
				    {noreply, State#state { menu=Menu }};
				{Cmd,Mod} ->
				    State1 = State#state { menu=Menu },
				    State2 = command(Cmd,State1#state.selected,
						     Mod,State),
				    invalidate(State2),
				    {noreply, State2}
			    end
		    end;
	       ?CTRL(State) ->
		    invalidate(State),
		    {noreply, State#state { operation = none,
					    selected = [],
					    pt1 = XY,
					    pt2 = XY }};

		?ALT(State) ->
		    {noreply, State#state { selected = [],
					    operation = none }};

	       true ->
		    Window = State#state.window,
		    epx:window_enable_events(Window,[motion]),
		    invalidate(State),
		    Sel0 = State#state.selected,
		    case select(State,XY,[]) of
			[] ->
			    Sel = if ?SHIFT(State) -> Sel0; true -> [] end,
			    {noreply,
			     State#state { operation = select,
					   selected = Sel,
					   pt1 = XY, pt2 = XY }};
			[Pos] ->
			    case lists:member(Pos,Sel0) of
				true ->
				    Sel = if ?SHIFT(State) ->
						  Sel0 -- [Pos];
					     true ->
						  Sel0
					  end,
				    {noreply,
				     State#state { operation = move,
						   selected = Sel,
						   pt1 = XY, pt2 = XY }};
				false ->
				    if ?SHIFT(State) ->
					    Selected = add_to_sel(Pos,Sel0),
					    {noreply,
					     State#state { operation = select,
							   selected = Selected,
							   pt1 = XY, pt2 = XY}};
				       true ->
					    {noreply,
					     State#state { operation = move,
							   selected = [Pos],
							   pt1 = XY, pt2 = XY}}
				    end
			    end
		    end
	    end;

	{button_release, [left], _Where={Xm,Ym,_Zm}} ->
	    XY = {Xm,Ym},
	    Window = State#state.window,
	    epx:window_disable_events(Window,[motion]),
	    State1 = State#state { pt = XY },
	    case State1#state.pt1 of
		undefined ->
		    {noreply,State1};
	       Pt1 ->
		    Pt2 = XY,
		    Sel0 = State1#state.selected,
		    case State1#state.operation of
			move ->
			    _Offset = coords_sub(Pt2, Pt1),
			    %% DNA = move_dna(Sel0,Offset,State1#state.dna),
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined, 
						     pt2 = undefined,
						     operation = none
						   }};
			select ->
			    Rect = coords_to_rect(Pt1,Pt2),
			    Sel1 = if ?SHIFT(State1) -> Sel0; true -> [] end,
			    Sel = select_area(State,Rect,Sel1),
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined,
						     pt2 = undefined,
						     operation = none,
						     selected = Sel }};
			_ ->
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined,
						     pt2 = undefined,
						     operation = none }}
		    end
	    end;

	{button_press, [right], _Where={X,Y,_Zm}} ->
	    io:format("press right enable menu\n", []),
	    %% MI = State#state.menu_info,
	    %% epx_gc:set_font(MI#menu_info.font),
	    %% Menu = menu(global),
	    %% MenuGeometry = graph_menu:calc_menu_size(Menu,MI),
	    epx:window_enable_events(State#state.window,[motion]),
	    Menu = epx_menu:set_row(State#state.menu, -1),
	    State1 = State#state { pt1 = {X,Y}, operation = menu,
				   menu = Menu },
	    invalidate(State1),
	    {noreply, State1};
			  
	{button_press,[wheel_down],{_Xm,_Ym,_Zm}} ->
	    flush_wheel(State#state.window),
	    State1 = move_down(State, 1),
	    %% Pos = State#state.pos + columns(State),
	    %% State1 = State#state { pos = Pos },
	    invalidate(State1),
	    {noreply, State1};
	{button_release,[wheel_down],_Coord} ->
	    {noreply, State};

	{button_press,[wheel_up],{_Xm,_Ym,_Zm}} ->
	    flush_wheel(State#state.window),
	    State1 = move_up(State, 1),
	    %% Pos = max(0, State#state.pos - columns(State)),
	    %% State1 = State#state { pos = Pos },
	    invalidate(State1),
	    {noreply, State1};
	{button_release,[wheel_up],_Coord} ->
	    {noreply, State};

	{button_press,[wheel_left],{_Xm,_Ym,_Zm}} ->
	    flush_wheel(State#state.window),
	    {noreply, State};
	{button_release,[wheel_left],_Coord} ->
	    {noreply, State};
	
	{button_press,[wheel_right],{_Xm,_Ym,_Zm}} ->
	    flush_wheel(State#state.window),
	    {noreply, State};
	{button_release,[wheel_right],_Coord} ->
	    {noreply, State};

	{motion, [], {Xm,Ym,_Zm}} ->
	    flush_motions(State#state.window),
	    if State#state.operation =:= menu ->
		    %% check menu row
		    {Row,Menu} = epx_menu:find_row(State#state.menu,
						   State#state.pt1,
						   {Xm,Ym}),
		    if Row =:= -1 ->
			    {noreply, State#state { menu=Menu }};
		       true ->
			    State1 = State#state { menu = Menu },
			    invalidate(State1),
			    {noreply, State1}
		    end;
	       true ->
		    {noreply, State}
	    end;

	{motion, [left], {Xm,Ym,_Zm}} ->
	    flush_motions(State#state.window),
	    XY = {Xm,Ym},
	    case State#state.pt1 of
		undefined ->
		    {noreply, State};
		_Pt1 ->
		    invalidate(State),
		    {noreply, State#state { pt2 = XY}}
	    end;

	{key_press, Sym, Mod, _code} ->
	    if Sym =:= $\e -> %% escape key processing
		    epx:window_disable_events(State#state.window,[motion]),
		    if not State#state.esc ->
			    {noreply, State#state { esc = true }};
		       true ->
			    {noreply, State}
		    end;
	       true ->
		    M = set_mod(State#state.keymod, Mod),
		    State1 = State#state { keymod=M },
		    State2 = command(Sym, State1#state.selected, M, State1),
		    invalidate(State2#state { esc = false }),
		    {noreply, State2}
	    end;

	{key_release, _Sym, Mod, _code} ->
	    %% %% io:format("Key release ~p mod=~p\n", [_Sym,Mod]),
	    M = clr_mod(State#state.keymod, Mod),
	    {noreply, State#state { keymod = M }};

	{configure, {_X,_Y,W,H}} ->
	    Zf = zoom_factor(State#state.zoom),
	    NRows    = nrows(State#state.winfo, H, Zf),
	    NColumns = ncolumns(State#state.winfo, W, Zf),
	    M = max(1, NColumns+State#state.noffs),
	    GW = trunc(M*Zf*?BASE_WIDTH),
	    GH = trunc(NRows*Zf*?BASE_HEIGHT),
	    Grid = resize_pixmap(State#state.grid,GW,GH,false),
	    Screen = resize_pixmap(State#state.screen,W,H,true),
	    
	    %% Avoid flicker?
	    epx:pixmap_copy_area(State#state.pixels,Screen,
				 0, 0, 0, 0,
				 State#state.width, State#state.height),
	    epx:pixmap_draw(Screen, State#state.window,
			    0, 0, 0, 0, W, H),
	    epx:sync(Screen,State#state.window),

	    Pixels = resize_pixmap(State#state.pixels,W,H,false),

	    State1 = State#state { screen = Screen,
				   pixels = Pixels,
				   grid   = Grid,
				   nrows  = NRows,
				   ncolumns = NColumns,
				   width=W, height=H,
				   gwidth = GW, gheight = GH
				 },
	    invalidate(State1),
	    %% maybe_invalidate(not ?USE_EXPOSURE, State1),
	    {noreply, State1};

	{expose, {_X,_Y,_W,_H}} ->
	    io:format("Expose x=~w,y=~w,w=~w,h=~w\n", [_X,_Y,_W,_H]),
	    State1 = draw(State),
	    {noreply, State1};
	
	redraw ->
	    flush_redraw(State),
	    State1 = try draw(State) of
			 S1 -> S1
		     catch
			 error:Reason:Stack ->
			     io:format("Error: ~p\n~p\n", [Reason,Stack]),
			     State
		     end,
	    {noreply, State1};

	close -> 
	    {stop,normal,State};

	destroy ->
	    epx:window_detach(State#state.window),
	    {stop,normal,State};

	_ ->
	    io:format("Epx event = ~p\n", [Event]),
	    {noreply,State}
    end.

%% update mod keys
set_mod(M, [shift|Mod]) ->  set_mod(M#keymod {shift = true}, Mod);
set_mod(M, [ctrl|Mod]) ->   set_mod(M#keymod {ctrl = true}, Mod);
set_mod(M, [alt|Mod]) ->    set_mod(M#keymod {alt = true}, Mod);
set_mod(M, [_|Mod]) ->      set_mod(M, Mod);
set_mod(M, []) -> M.

clr_mod(M, [shift|Mod]) ->  clr_mod(M#keymod {shift = false}, Mod);
clr_mod(M, [ctrl|Mod]) ->   clr_mod(M#keymod {ctrl = false}, Mod);
clr_mod(M, [alt|Mod]) ->    clr_mod(M#keymod {alt = false}, Mod);
clr_mod(M, [_|Mod]) ->      clr_mod(M, Mod);
clr_mod(M, []) -> M.


%%
%% key commands:
%%   up / down / left / right move selected dna
%%   ctrl 0 - 9               set color on selected vertices
%%   alt  0 - 9               set shape on selected vertices
%%   \b                       delete selected vertices
%%   ctrl x                   cut selected vertices to clipboard
%%   ctrl c                   copy selected vertices to clipboard
%%   ctrl v                   paste graph from clipboard
%%   +                        zoom in
%%   -                        zoom out
%%
command(up, _Selected, _Mod, State) ->
    move_up(State,1);
command($p, _Selected, Mod, State) when Mod#keymod.ctrl ->
    move_up(State, 1);
command(pageup, _Selected, _Mod, State) ->
    move_up(State,rows(State));
%% command($v, _Selected, Mod, State) when State#state.esc ->
%%    move_up(State,rows(State));
command(down, _Selected, _Mod, State) ->
    move_down(State,1);
command($n, _Selected, Mod, State) when Mod#keymod.ctrl ->
    move_down(State,1);
command(pagedown, _Selected, _Mod, State) ->
    move_down(State,rows(State));
%%command($v, _Selected, Mod, State) when Mod#keymod.ctrl ->
%%    move_down(State,rows(State));
command(left, _Selected, Mod, State) ->
    if Mod#keymod.alt -> %% adjust ncolumns  %% ?MIN_OFFSET..0
	    State1=State#state { noffs = max(?MIN_OFFSET,State#state.noffs-1)},
	    M = columns(State1),
	    Zf = zoom_factor(State1#state.zoom),
	    GWidth = trunc(M*Zf*?BASE_WIDTH),
	    State1#state { gwidth = GWidth };	    
       true ->
	    Pos = max(0, State#state.pos - 1),
	    State#state { pos = Pos }
    end;
command(right, _Selected, Mod, State) ->
    if Mod#keymod.alt -> %% adjust ncolumns  %% -16 0
	    State1 = State#state { noffs = min(0, State#state.noffs+1) },
	    M = columns(State1),
	    Zf = zoom_factor(State1#state.zoom),
	    GWidth = trunc(M*Zf*?BASE_WIDTH),
	    State1#state { gwidth = GWidth };
       true ->
	    Pos = State#state.pos + 1,  %% fixme limit upwards!
	    State#state { pos = Pos }
    end;
command(insert, Selected, Mod, State) ->
    %% insert Z base after/before the selected base(s)
    DNA = 
	if Mod#keymod.alt ->
		insert_data_before(Selected,State#state.dna,<<$Z>>);
	   true ->
		insert_data_after(Selected,State#state.dna,<<$Z>>)
	end,
    State#state { dna = DNA };

command($a, Selected, Mod, State) when not Mod#keymod.ctrl ->
    DNA = replace_data(Selected, State#state.dna, <<$A>>),
    State#state { dna = DNA };
command($c, Selected, Mod, State) when not Mod#keymod.ctrl ->
    DNA = replace_data(Selected, State#state.dna, <<$C>>),
    State#state { dna = DNA };
command($g, Selected, Mod, State) when not Mod#keymod.ctrl ->
    DNA = replace_data(Selected, State#state.dna, <<$G>>),
    State#state { dna = DNA };
command($t, Selected, Mod, State)  when not Mod#keymod.ctrl ->
    DNA = replace_data(Selected, State#state.dna, <<$T>>),
    State#state { dna = DNA };
command($z, Selected, Mod, State)  when not Mod#keymod.ctrl ->
    DNA = replace_data(Selected, State#state.dna, <<$Z>>),
    State#state { dna = DNA };
command($\b, Selected, _Mod, State) ->
    DNA = remove_data(Selected, State#state.dna),
    State#state { dna = DNA, selected = [] };
command($x, Selected, Mod, State) when Mod#keymod.ctrl ->
    {DNA,Clip} = cut_data(Selected, State#state.dna),
    State#state { dna=DNA, clip=Clip, selected = [] };
command($c, Selected, Mod, State) when Mod#keymod.ctrl ->
    Clip = copy_data(Selected, State#state.dna),
    State#state { clip=Clip };
command($v, Selected, Mod, State) when Mod#keymod.ctrl ->
    DNA = paste_data(Selected, State#state.dna, State#state.clip),
    State#state { dna=DNA, selected = [] };
command($t, Selected, Mod, State) when Mod#keymod.ctrl ->
    DNA = case Selected of 
	      [] -> 
		  State#state.dna;
	      [Pos] ->
		  toggle_data([Pos,Pos+1],State#state.dna);
	      _ ->
		  toggle_data(Selected, State#state.dna)
	  end,
    State#state { dna=DNA };
command($s, _Selected, Mod, State) when Mod#keymod.ctrl ->
    %% SAVE
    State;
command($+, _Selected, _Mod, State) ->
    Zoom = min(State#state.zoom + 1, ?MAX_ZOOM),
    Zf = zoom_factor(Zoom),
    NRows = nrows(State#state.winfo,State#state.height, Zf),
    NColumns = ncolumns(State#state.winfo,State#state.width, Zf),
    M = max(1, NColumns+State#state.noffs),
    GW = trunc(M*Zf*?BASE_WIDTH),
    GH = trunc(NRows*Zf*?BASE_HEIGHT),
    Grid = resize_pixmap(State#state.grid,GW,GH,false),
    set_base_font(State#state { zoom = Zoom, 
				grid = Grid,
				gwidth = GW, gheight = GH,
				nrows=NRows, ncolumns=NColumns });
command($-, _Selected, _Mod, State) ->
    Zoom = max(State#state.zoom - 1, ?MIN_ZOOM),
    Zf = zoom_factor(Zoom),
    NRows = nrows(State#state.winfo,State#state.height, Zf),
    NColumns = ncolumns(State#state.winfo,State#state.width, Zf),
    M = max(1, NColumns+State#state.noffs),
    GW = trunc(M*Zf*?BASE_WIDTH),
    GH = trunc(NRows*Zf*?BASE_HEIGHT),
    Grid = resize_pixmap(State#state.grid,GW,GH,false),
    set_base_font(State#state { zoom = Zoom, 
				grid = Grid,
				gwidth = GW, gheight = GH,
				nrows=NRows, ncolumns=NColumns });

command($1, _Selected, Mod, State) when Mod#keymod.ctrl ->
    State#state { view = base };
command($2, _Selected, Mod, State) when Mod#keymod.ctrl ->
    State#state { view = amino };
command($3, _Selected, Mod, State) when Mod#keymod.ctrl ->
    State#state { view = short };
command($4, _Selected, Mod, State) when Mod#keymod.ctrl ->
    State#state { view = compressed };

command(Command, _Selected, _Mod, State) ->
    io:format("Command = ~p\n", [Command]),
    State.

move_up(State,N) ->
    Pos = max(0, State#state.pos - N*columns(State)),
    DRows = State#state.drows + N,
    State#state { pos = Pos, drows = DRows, ddir = -1 }.

move_down(State,N) ->
    Pos = State#state.pos + N*columns(State),  %% fixme limit upwards!
    DRows = State#state.drows + N,
    State#state { pos = Pos, drows = DRows, ddir = 1 }.

nrows(WI,H,Zf) ->
    Height = H - (?TOP_OFFSET + ?BOTTOM_OFFSET + WI#window_info.bottom_bar),
    max(1, trunc(Height / (Zf*?BASE_HEIGHT))).

%% ncolumns is a multiple of 3
ncolumns(_WI,W,Zf) ->
    Width = W - (?LEFT_OFFSET + ?RIGHT_OFFSET),
    max(1, 3*trunc(Width / (3*Zf*?BASE_WIDTH))).

rows(State) ->
    State#state.nrows.

columns(State) ->
    max(1, State#state.ncolumns+State#state.noffs).

%% load #profile from environment
load_profile(E) ->
    D = #profile{},
    Zoom = load_value(zoom, E, D),
    %% Special case - fixme!!!
    {Width,Height} =
	case proplists:get_value(size, E, unset) of
	    unset ->
		{load_value(base_width, E, D),load_value(base_height, E, D)};
	    Size ->
		{Size, Size}
	end,
    S = load_value(scheme, E, D),
    #profile {
       scheme          = S,
       zoom            = Zoom,
       screen_color    = load_color(S,screen_color, E, D),
       selection_alpha = load_value(selection_alpha, E, D),
       selection_color = load_color(S,selection_color, E, D),
       selection_border_width = load_value(selection_border_width, E, D),
       selection_border_color = load_color(S,selection_border_color, E, D),
       base_width       = Width,
       base_height      = Height,
       border_width     = load_value(border_width, E, D),
       border_color     = load_color(S,border_color, E, D),
       select_color     = load_color(S,select_color, E, D),
       select_border_width = load_value(select_border_width,E,D),
       select_border_color = load_color(S,select_border_color,E,D),
       highlight_color     = load_color(S,highlight_color,E,D),
       highlight_border_width = load_value(highlight_border_width,E,D),
       highlight_border_color = load_color(S,highlight_border_color,E,D),
       label_font = load_value(label_font, E, D),
       label_font_size = load_value(label_font_size, E, D),
       label_font_color = load_color(S,label_font_color,E,D),
       menu_font_name = load_value(menu_font_name, E, D),
       menu_font_size = load_value(menu_font_size, E, D),
       menu_font_color = load_color(S,menu_font_color,E,D),
       menu_background_color = load_color(S,menu_background_color,E,D),
       menu_border_color = load_color(S,menu_border_color,E,D)
      }.

%% map of profile record names and indices in the record
profile_index() ->
    maps:from_list(lists:zip(record_info(fields, profile), 
			     lists:seq(2,record_info(size,profile)))).

%% load value from environment or profile
load_value(Key, Env, Profile) ->
    case proplists:get_value(Key, Env) of
	undefined ->
	    Ix = profile_index(),
	    Pos = maps:get(Key, Ix),
	    element(Pos, Profile);
	Value ->
	    Value
    end.

%% load RGB value from environment or profile
load_color(Scheme, Key, Env, Profile) ->
    Value = load_value(Key, Env, Profile),
    epx_profile:color(Scheme, Value).

color32({R,G,B}) -> (R bsl 16)+(G bsl 8)+(B);
color32({A,R,G,B}) -> (A bsl 24)+(R bsl 16)+(G bsl 8)+(B).

alpha_color(A,{_,R,G,B}) -> {A,R,G,B};
alpha_color(A,{R,G,B}) -> {A,R,G,B};
alpha_color(A,Name) when is_list(Name); is_atom(Name) ->
    alpha_color(A, epx_color:from_name(Name)).

add_color({R0,G0,B0},{R1,G1,B1}) ->
    {max(255, R0+R1),max(255, G0+G1),max(255, B0+B1)};
add_color({A0,R0,G0,B0},{A1,R1,G1,B1}) ->
    {max(255, A0+A1),max(255, R0+R1),max(255, G0+G1),max(255, B0+B1)}.

add_color(C1,C2,C3) ->
    add_color(add_color(C1,C2),C3).

add_color(C1,C2,C3,C4) ->
    add_color(add_color(add_color(C1,C2),C3),C4).

create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

create_window_profile(Profile) ->
    #window_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.window_font_name,
       font_size        = Profile#profile.window_font_size,
       font_color       = Profile#profile.window_font_color,
       scroll_bar_color = Profile#profile.scroll_bar_color,
       scroll_hndl_color = Profile#profile.scroll_hndl_color,
       scroll_horizontal = Profile#profile.scroll_horizontal,
       scroll_vertical   = Profile#profile.scroll_vertical,
       top_bar_color     = Profile#profile.top_bar_color,
       left_bar_color    = Profile#profile.left_bar_color,
       right_bar_color   = Profile#profile.right_bar_color,
       bottom_bar_color  = Profile#profile.bottom_bar_color
      }.

resize_pixmap(undefined, W, H, Attached) ->
    Pixmap = next_pixmap(W,H),
    if Attached ->
	    epx:pixmap_attach(Pixmap);
       true ->
	    ok
    end,
    Pixmap;
resize_pixmap(Pixmap, W, H, Attached) ->
    case epx:pixmap_info(Pixmap,[width,height]) of
	[{width,PW},{height,PH}] when PW < W; PH < H ->
	    if Attached ->
		    epx:pixmap_detach(Pixmap),
		    Pixmap1 = next_pixmap(W,H),
		    epx:pixmap_attach(Pixmap1),
		    Pixmap1;
	       true ->
		    next_pixmap(W,H)
	    end;
	_ ->
	    Pixmap
    end.

next_pixmap(W,H) ->
    NPW = 1 bsl ceil(math:log2(W)),
    NPH = 1 bsl ceil(math:log2(H)),
    epx:pixmap_create(NPW, NPH, argb).

%% cut selected positions from data and
%% return edited data + copy of the data cut out.
cut_data(Selected, Data) ->
    Clip = copy_data(Selected,Data),
    {remove_data(Selected,Data),Clip}.

%% Copy selected data positions 
copy_data(Selected,Data) ->
    list_to_binary([get_base(Pos,Data) || Pos <- Selected]).

%% Remove selected data - remove from high positions so
%% that offsets does not have to be recalculated
remove_data(PosList, Data) ->
    remove_data_(lists:reverse(PosList), Data).

remove_data_([Pos|PosList], Data) ->
    <<Before:Pos/binary, _, After/binary>> = Data,
    remove_data_(PosList, <<Before/binary, After/binary>>);
remove_data_([], Data) -> Data.

%% Pase Clip onto every Selected position
paste_data(PosList,Data,Clip) ->
    paste_data_(PosList,0,Data,Clip).
    
paste_data_([Pos|PosList],Offs,Data,Clip) ->
    Pos1 = Pos + Offs,
    <<Before:Pos1/binary, _, After/binary>> = Data,
    paste_data_(PosList,Offs+byte_size(Clip)-1,
		<<Before/binary,Clip/binary,After/binary>>,Clip);
paste_data_([], _, Data, _Clip) ->
    Data.

insert_data_before(PosList,Data,Clip) ->
    insert_data_before_(PosList,0,Data,Clip).
    
insert_data_before_([Pos|PosList],Offs,Data,Clip) ->
    Pos1 = Pos + Offs,
    <<Before:Pos1/binary, After/binary>> = Data,
    insert_data_before_(PosList,Offs+byte_size(Clip),
			<<Before/binary,Clip/binary,After/binary>>,Clip);
insert_data_before_([], _, Data, _Clip) ->
    Data.

insert_data_after(PosList,Data,Clip) ->
    insert_data_after_(PosList,0,Data,Clip).
    
insert_data_after_([Pos|PosList],Offs,Data,Clip) ->
    Pos1 = Pos + Offs + 1,
    <<Before:Pos1/binary, After/binary>> = Data,
    insert_data_after_(PosList,Offs+byte_size(Clip),
		       <<Before/binary,Clip/binary,After/binary>>,Clip);
insert_data_after_([], _, Data, _Clip) ->
    Data.

toggle_data(PosList, Data) ->
    N = length(PosList),
    case lists:split(N div 2, PosList) of
	{A,B} when N band 1 =:= 0 ->  
	    swap_data(A, lists:reverse(B), Data);
	{A,[_|B]} ->
	    swap_data(A, lists:reverse(B), Data)
    end.

swap_data([PosA|PosListA], [PosB|PosListB], Data) when PosA < PosB ->
    Len = (PosB - PosA)-1,
    <<Before:PosA/binary, A, Middle:Len/binary, B, After/binary>> = Data,
    Data1 = <<Before/binary, B, Middle/binary, A, After/binary>>,
    swap_data(PosListA, PosListB, Data1);
swap_data([], [], Data) ->
    Data.

%%
%%  replace: replace selected positions
%%  with data from Bin, rotate Bin if
%%  Bin becomes empty
%%

replace_data(_PosList,Data,<<>>) ->
    Data;
replace_data(PosList,Data,Bin) ->
    replace_data_(PosList,Data,Bin,Bin).

replace_data_(PosList,Data,<<>>,Bin0) ->
    replace_data_(PosList,Data,Bin0,Bin0);
replace_data_([Pos|PosList],Data,<<B,Bin/binary>>,Bin0) ->
    <<Before:Pos/binary, _, After/binary>> = Data,
    replace_data_(PosList,<<Before/binary,B,After/binary>>, Bin, Bin0);
replace_data_(_, Data, _, _Bin0) ->
    Data.

add_to_sel(Pos, Selected) ->
    lists:sort([Pos|Selected]).

select(State,{X,Y},_Selected) ->
    if X =< ?LEFT_OFFSET ->
	    [];
       X >= State#state.width - ?RIGHT_OFFSET -> 
	    [];
       Y =< ?TOP_OFFSET ->
	    [];
       Y >= State#state.height - ?BOTTOM_OFFSET ->
	    [];
       true ->
	    Zf = zoom_factor(State#state.zoom),
	    %% translate XY into I,J and then POS
	    I = trunc((Y-?TOP_OFFSET) / (Zf*?BASE_HEIGHT)),
	    J = trunc((X-?LEFT_OFFSET) / (Zf*?BASE_WIDTH)),
	    Pos = State#state.pos + I*columns(State) + J,
	    %% io:format("X=~w,Y=~w,I=~w,J=~w,Pos=~w\n", [X,Y,I,J,Pos]),
	    [Pos]
    end.

select_area(_State,_Rect,Selected) ->
    %% FIXME:
    Selected.


rect_offset({X,Y,W,H}, {X1,Y1}) ->
    {X+X1,Y+Y1,W,H}.

coords_sub({X1,Y1},{X0,Y0}) ->
    {X1-X0,Y1-Y0}.

coords_add({X1,Y1},{X0,Y0}) ->
    {X1+X0,Y1+Y0}.

coords_to_rect({X0,Y0},{X1,Y1}) ->
    X = min(X0,X1),
    Y = min(Y0,Y1),
    W = abs(X1-X0) + 1,
    H = abs(Y1-Y0) + 1,
    {X,Y,W,H}.

rect_overlap(R1,R2) ->
    case epx_rect:intersect(R1, R2) of
	{_,_,0,0} -> false;
	_ -> true
    end.

point_in_rect({X1,Y1}, {X2,Y2,W,H}) ->
    (X1 >= X2) andalso (X1 =< X2+W-1) andalso
    (Y1 >= Y2) andalso (Y1 =< Y2+H-1).

maybe_invalidate(true, State) ->
    invalidate(State);
maybe_invalidate(_, _) ->
    ok.

invalidate(State) ->
    self() ! {epx_event, State#state.window, redraw}.

flush_redraw(State) ->
    receive
	{epx_event, Window, redraw} when State#state.window =:= Window ->
	    flush_redraw(State)
    after 0 ->
	    State
    end.

menu(global) ->
    [
     {"Cut", "Ctrl+X"},
     {"Copy", "Ctrl+C"},
     {"Paste", "Ctrl+V"},
     {"Delete", "Del"},
     {"---", ""},
     {"Save",  "Ctrl+S"},
     {"---", ""},
     {"Zoom in", "+"},
     {"Zoom out", "-"},
     {"---", ""},
     {"View Base", "Ctrl+1"},
     {"View Amino", "Ctrl+2"},
     {"View Short", "Ctrl+3"},
     {"View Compressed", "Ctrl+4"}
    ].

%%
%% FIXME:
%%   keep track on scroll up and scroll down
%%   for wheel-up / wheel-down / key-up / key-down
%%

draw(State = #state { profile = Profile }) ->
    Zf = zoom_factor(State#state.zoom),
    Scheme = Profile#profile.scheme,
    ScreenColor = epx_profile:color(Scheme, Profile#profile.screen_color),
    epx:pixmap_fill(State#state.pixels,ScreenColor),
    %% epx:pixmap_fill(State#state.grid,ScreenColor),
    epx_gc:set_fill_style(solid),
    State1 = draw_dna(State, Zf, Scheme, Profile),  %% into grid!
    %% FIXME: must use dpos!
    draw_selected(State, Zf, Scheme, Profile),      %% into grid!
    case State1#state.dpos of
	0 ->
	    epx:pixmap_copy_area(State#state.grid, State#state.pixels,
				 0, 0, ?LEFT_OFFSET, ?TOP_OFFSET,
				 State#state.gwidth,
				 State#state.gheight);
	DP ->
	    TopH = Zf*DP*?BASE_HEIGHT,
	    BotH = State#state.gheight - TopH,
	    epx:pixmap_copy_area(State#state.grid, State#state.pixels,
				 0, 0, ?LEFT_OFFSET, ?TOP_OFFSET+BotH,
				 State#state.gwidth,
				 TopH),
	    epx:pixmap_copy_area(State#state.grid, State#state.pixels,
				 0, TopH, ?LEFT_OFFSET, ?TOP_OFFSET,
				 State#state.gwidth,
				 BotH)
    end,
    draw_bottom_bar(State),                     %% into pixels
    if State#state.pt1 =/= undefined, State#state.operation =:= menu ->
	    io:format("draw menu\n", []),
	    epx_menu:draw(State#state.menu, State#state.pixels,
			  State#state.pt1);
       true ->
	    ok
    end,
    %% copy pixels to screen pixel
    epx:pixmap_copy_to(State#state.pixels,State#state.screen),
%%    epx:pixmap_copy_area(State#state.pixels,State#state.screen,
%%			 0, 0, 0, 0, 
%%			 State#state.width, State#state.height),
    epx:pixmap_draw(State#state.screen, State#state.window,
		    0, 0, 0, 0,
		    State#state.width, State#state.height),
    epx:sync(State#state.screen,State#state.window),
    State1.

%%
%% Draw DNA from top to bottom
%%
draw_rows(State) ->
    NRows = rows(State),
    DRows = State#state.drows,
    if DRows =:= 0 -> NRows;
       DRows > NRows -> NRows;
       true -> DRows
    end.

draw_dna(State, Zf, Scheme, Profile) ->
    N = draw_rows(State),
    io:format("Draw ~w rows dpos=~w\n", [N, State#state.dpos]),
    draw_dna(State,Zf,Scheme,Profile,0,N).

draw_dna(State,Zf,Scheme,Profile,I,NRows) ->
    case State#state.font_used of
	undefined -> ignore;
	#fi{ font=F } ->
	    epx_gc:set_font(F)
    end,
    if Zf < 0.5 ->
	    epx_gc:set_border_width(0); %% see the color better
       true ->
	    epx_gc:set_border_width(zm(Profile#profile.border_width,Zf)),
	    epx_gc:set_border_color(
	      epx_profile:color(Scheme,Profile#profile.border_color))
    end,
    M  = columns(State),
    N  = rows(State),
    P0 = State#state.pos + I*M,
    DP = State#state.dpos,
    DD = State#state.ddir,
    DP1 = case State#state.view of
	      base ->
		  draw_base(State,NRows,P0,I,DP,DD,N,M,Zf);
	      short ->
		  draw_short(State,NRows,P0,I,DP,DD,N,M,Zf);
	      amino ->
		  draw_amino(State,NRows,P0,I,DP,DD,N,M,Zf);
	      compressed ->
		  draw_compressed(State,NRows,P0,I,DP,DD,N,M,Zf)
	  end,
    io:format("dpos' = ~w\n", [DP1]),
    State#state { dpos = DP1, drows = 0, ddir = 0 }.


-record(ci,
	{ amino,
	  short,
	  compressed,
	  color
	}).

-define(CI(A,S,Z,C), #ci{amino=(A),short=(S),compressed=(Z),color=(C)}).
-define(CN(A,B,C), [A,B,C]).
-define(A, $A).
-define(C, $C).
-define(G, $G).
-define(U, $T).

%% color coded property
-define(nonpolar, 16#ffe75f).
-define(polar,    16#b3dec0).
-define(basic,    16#bbbfe0).
-define(acidic,   16#f8b7d3).
-define(start,    16#00ff00).
-define(stop,     16#b0b0b0).
-define(none,     16#000000).

-define(DEF, ?CI("Non", "N", "NNN", ?none)).  %% PCR read error?

codon() ->
    #{
      ?CN(?G,?C,?U) => ?CI("Ala","A","GCN",?nonpolar),
      ?CN(?G,?C,?C) => ?CI("Ala","A","GCN",?nonpolar),
      ?CN(?G,?C,?A) => ?CI("Ala","A","GCN",?nonpolar),
      ?CN(?G,?C,?G) => ?CI("Ala","A","GCN",?nonpolar),
      ?CN(?C,?G,?U) => ?CI("Arg","R","CGN",?basic),
      ?CN(?C,?G,?C) => ?CI("Arg","R","CGN",?basic),
      ?CN(?C,?G,?A) => ?CI("Arg","R","CGN",?basic),
      ?CN(?C,?G,?G) => ?CI("Arg","R","CGN",?basic),
      ?CN(?A,?G,?A) => ?CI("Arg","R","AGR",?basic),
      ?CN(?A,?G,?G) => ?CI("Arg","R","AGR",?basic),
      ?CN(?A,?A,?U) => ?CI("Asn","N","AAY",?polar),
      ?CN(?A,?A,?C) => ?CI("Asn","N","AAY",?polar),
      ?CN(?G,?A,?U) => ?CI("Asp","D","GAY",?acidic),
      ?CN(?G,?A,?C) => ?CI("Asp","D","GAY",?acidic),
      ?CN(?U,?G,?U) => ?CI("Cys","C","UGY",?polar),
      ?CN(?U,?G,?C) => ?CI("Cys","C","UGY",?polar),
      ?CN(?C,?A,?A) => ?CI("Gln","Q","CAR",?polar),
      ?CN(?C,?A,?G) => ?CI("Gln","Q","CAR",?polar),
      ?CN(?G,?A,?A) => ?CI("Glu","E","GAR",?acidic),
      ?CN(?G,?A,?G) => ?CI("Glu","E","GAR",?acidic),
      ?CN(?G,?G,?U) => ?CI("Gly","G","GGN",?nonpolar),
      ?CN(?G,?G,?C) => ?CI("Gly","G","GGN",?nonpolar),
      ?CN(?G,?G,?A) => ?CI("Gly","G","GGN",?nonpolar),
      ?CN(?G,?G,?G) => ?CI("Gly","G","GGN",?nonpolar),
      ?CN(?C,?A,?U) => ?CI("His","H","CAY",?basic),
      ?CN(?C,?A,?C) => ?CI("His","H","CAY",?basic),
      ?CN(?A,?U,?U) => ?CI("Ile","I","AUH",?nonpolar),
      ?CN(?A,?U,?C) => ?CI("Ile","I","AUH",?nonpolar),
      ?CN(?A,?U,?A) => ?CI("Ile","I","AUH",?nonpolar),
      ?CN(?U,?U,?A) => ?CI("Leu","L","UUR",?nonpolar),
      ?CN(?U,?U,?G) => ?CI("Leu","L","UUR",?nonpolar),
      ?CN(?C,?U,?U) => ?CI("Leu","L","CUN",?nonpolar), 
      ?CN(?C,?U,?C) => ?CI("Leu","L","CUN",?nonpolar),
      ?CN(?C,?U,?A) => ?CI("Leu","L","CUN",?nonpolar),
      ?CN(?C,?U,?G) => ?CI("Leu","L","CUN",?nonpolar),
      ?CN(?A,?A,?A) => ?CI("Lys","K","AAR",?basic),
      ?CN(?A,?A,?G) => ?CI("Lys","K","AAR",?basic),
      ?CN(?U,?U,?U) => ?CI("Phe","F","UUY",?nonpolar),
      ?CN(?U,?U,?C) => ?CI("Phe","F","UUY",?nonpolar),
      ?CN(?C,?C,?U) => ?CI("Pro","P","CCN",?nonpolar),
      ?CN(?C,?C,?C) => ?CI("Pro","P","CCN",?nonpolar),
      ?CN(?C,?C,?A) => ?CI("Pro","P","CCN",?nonpolar),
      ?CN(?C,?C,?G) => ?CI("Pro","P","CCN",?nonpolar),
      ?CN(?U,?C,?U) => ?CI("Ser","S","UCN",?polar),
      ?CN(?U,?C,?C) => ?CI("Ser","S","UCN",?polar),
      ?CN(?U,?C,?A) => ?CI("Ser","S","UCN",?polar),
      ?CN(?U,?C,?G) => ?CI("Ser","S","UCN",?polar),
      ?CN(?A,?G,?U) => ?CI("Ser","S","AGY",?polar),
      ?CN(?A,?G,?C) => ?CI("Ser","S","AGY",?polar),
      ?CN(?A,?C,?U) => ?CI("Thr","T","ACN",?polar),
      ?CN(?A,?C,?C) => ?CI("Thr","T","ACN",?polar),
      ?CN(?A,?C,?A) => ?CI("Thr","T","ACN",?polar),
      ?CN(?A,?C,?G) => ?CI("Thr","T","ACN",?polar),
      ?CN(?U,?G,?G) => ?CI("Trp","W","UGG",?nonpolar),
      ?CN(?U,?A,?U) => ?CI("Tyr","Y","UAY",?polar),
      ?CN(?U,?A,?C) => ?CI("Tyr","Y","UAY",?polar),
      ?CN(?G,?U,?U) => ?CI("Val","V","GUN",?nonpolar),
      ?CN(?G,?U,?C) => ?CI("Val","V","GUN",?nonpolar),
      ?CN(?G,?U,?A) => ?CI("Val","V","GUN",?nonpolar),
      ?CN(?G,?U,?G) => ?CI("Val","V","GUN",?nonpolar),
      ?CN(?A,?U,?G) => ?CI("STA","^","AUG",?start),
      %% ?CN(?A,?U,?G) => ?CI("Met","M","AUG"),
      ?CN(?U,?A,?A) => ?CI("STP","$","URA",?stop),
      ?CN(?U,?G,?A) => ?CI("STP","$","URA",?stop),
      ?CN(?U,?A,?G) => ?CI("STP","$","UAG",?stop)
     }.

complement($A) -> $T;
complement($C) -> $G;
complement($G) -> $C;
complement($T) -> $A;
complement(L) when is_list(L) ->
    [complement(B) || B <- L].

amino([A,B,C | L]) ->
    CI = maps:get(?CN(A,B,C), codon(), ?DEF),
    [M,N,O] = CI#ci.amino,
    [M, N, O | amino(L)];
amino(_) ->
    [].

%%
%% Draw DNA as separate bases
%%
draw_base(_State,0,_Pos,_I,DP,_DD,N,_M,_Zf) ->
    mod(DP, N);
draw_base(State,K,Pos,I,DP,DD,N,M,Zf) ->
    Row = get_row(Pos,State#state.dna,M),
    DP1 = mod(DP,N),
    Y = DP1*?BASE_HEIGHT,
    draw_base_row(State,Row,Y,0,Zf),
    draw_base(State,K-1,Pos+M,I+1,DP1+DD,DD,N,M,Zf).

draw_base_row(State,<<Base,Row/binary>>,Y,X,Zf) ->
    Rect = scale_rect(X,Y,?BASE_WIDTH,?BASE_HEIGHT, Zf),
    RGB = element(Base, State#state.color_map),
    epx_gc:set_fill_color(RGB),
    epx:draw_rectangle(State#state.grid,Rect),
    draw_name1(State,Rect,[Base]),
    draw_base_row(State,Row,Y,X+?BASE_WIDTH,Zf);
draw_base_row(_State,<<>>,_Y,_X,_Zf) ->
    ok.
%%
%% Draw amino codon with short name
%%
draw_short(_State,0,_Pos,_I,DP,_DD,N,_M,_Zf) ->
    mod(DP, N);
draw_short(State,K,Pos,I,DP,DD,N,M,Zf) ->
    Row = get_row(Pos,State#state.dna,M),
    DP1 = mod(DP,N),
    Y = DP1*?BASE_HEIGHT,
    draw_short_row(State,Row,Y,0,Zf),
    draw_short(State,K-1,Pos+M,I+1,DP1+DD,DD,N,M,Zf).

draw_short_row(State,<<B1,B2,B3,Row/binary>>,Y,X,Zf) ->
    CI = maps:get(?CN(B1,B2,B3), codon(), ?DEF),
    Rect = scale_rect(X,Y,?BASE_WIDTH,?BASE_HEIGHT, Zf),
    epx_gc:set_fill_color(CI#ci.color),
    epx:draw_rectangle(State#state.grid,Rect),
    draw_name1(State,Rect,CI#ci.short),
    draw_short_row(State,Row,Y,X+?BASE_WIDTH,Zf);
draw_short_row(_State,<<>>,_Y,_X,_Zf) ->
    ok.

%%
%% Draw DNA with amino compressed name 3*char
%%
draw_compressed(_State,0,_Pos,_I,DP,_DD,N,_M,_Zf) ->    
    mod(DP, N);
draw_compressed(State,K,Pos,I,DP,DD,N,M,Zf) ->
    Row = get_row(Pos,State#state.dna,M),
    DP1 = mod(DP,N),
    Y = DP1*?BASE_HEIGHT,
    draw_compressed_row(State,Row,Y,0,Zf),
    draw_compressed(State,K-1,Pos+M,I+1,DP1+DD,DD,N,M,Zf).

draw_compressed_row(State,<<B1,B2,B3,Row/binary>>,Y,X,Zf) ->
    CI = maps:get(?CN(B1,B2,B3), codon(), ?DEF),
    Rect = scale_rect(X,Y,3*?BASE_WIDTH,?BASE_HEIGHT, Zf),
    epx_gc:set_fill_color(CI#ci.color),
    epx:draw_rectangle(State#state.grid,Rect),
    draw_name3(State,Rect,CI#ci.compressed),
    draw_compressed_row(State,Row,Y,X+3*?BASE_WIDTH,Zf);
draw_compressed_row(_State,<<>>,_Y,_X,_Zf) ->
    ok.

%%
%% Draw DNA with amino acid names
%%
draw_amino(_State,0,_Pos,_I,DP,_DD,N,_M,_Zf) ->    
    mod(DP, N);
draw_amino(State,K,Pos,I,DP,DD,N,M,Zf) ->
    Row = get_row(Pos,State#state.dna,M),
    DP1 = mod(DP,N),
    Y = DP1*?BASE_HEIGHT,
    draw_amino_row(State,Row,Y,0,Zf),
    draw_amino(State,K-1,Pos+M,I+1,DP1+DD,DD,N,M,Zf).

draw_amino_row(State,<<B1,B2,B3,Row/binary>>,Y,X,Zf) ->
    CI = maps:get(?CN(B1,B2,B3), codon(), ?DEF),
    Rect = scale_rect(X,Y,3*?BASE_WIDTH,?BASE_HEIGHT, Zf),
    epx_gc:set_fill_color(CI#ci.color),
    epx:draw_rectangle(State#state.grid,Rect),
    draw_name3(State,Rect,CI#ci.amino),
    draw_amino_row(State,Row,Y,X+3*?BASE_WIDTH,Zf);
draw_amino_row(_State,<<>>,_Y,_X,_Zf) ->
    ok.


%% draw string with one letter
draw_name1(State, {X,Y,W,H}, Name=[_]) ->
    case State#state.font_used of
	undefined ->
	    ignore;
	#fi { width=GW, height=GH, ascent=GA } ->
	    %% io:format("label=~s, rect=~w\n", [[Char], R]),
	    X1 = X + trunc(W - GW) div 2,
	    Y1 = Y + trunc(H - GH) div 2,
	    epx_gc:set_foreground_color(?TEXT_COLOR),
	    %% epx_gc:set_foreground_color(State#state.fcolor),
	    epx:draw_string(State#state.grid, X1, Y1+1+GA, Name)
    end.

%% string with 3-letters
draw_name3(State, {X,Y,W,H}, Amino=[_,_,_]) ->
    case State#state.font_used of
	undefined ->
	    ignore;
	#fi { width=GW, height=GH, ascent=GA } ->
	    %% io:format("label=~s, rect=~w\n", [[Char], R]),
	    X1 = X + trunc(W - 3*GW) div 2,
	    Y1 = Y + trunc(H - GH) div 2,
	    epx_gc:set_foreground_color(?TEXT_COLOR),
	    %% epx_gc:set_foreground_color(State#state.fcolor),
	    epx:draw_string(State#state.grid, X1, Y1+1+GA, Amino)
    end.

%%
%% Draw selected bases
%%

draw_selected(State, Zf, Scheme, Profile) ->
    Start = State#state.pos,
    Stop  = Start + rows(State)*columns(State)-1,
    SelectColor = epx_profile:color(Scheme,Profile#profile.select_color),
    epx_gc:set_border_width(zm(Profile#profile.selection_border_width,Zf)),
    epx_gc:set_border_color(
      epx_profile:color(Scheme,Profile#profile.selection_border_color)),
    epx_gc:set_fill_color(SelectColor),
    draw_selected_(State, State#state.selected, Start, Stop, Zf).

draw_selected_(State, [Pos|Selected], Start, Stop, Zf) when
      Pos >= Start, Pos =< Stop ->
    Pos0 = Pos - Start,  %% relative to upper left corder
    M = columns(State),
    I = Pos0 div M,
    J = Pos0 rem M,
    X0 = J*?BASE_WIDTH,
    Y0 = I*?BASE_HEIGHT,
    {X,Y,W,H} = scale_rect(X0,Y0,?BASE_WIDTH-1,?BASE_HEIGHT-1,Zf),
    SH = max(1, trunc(?SEL_HEIGHT*Zf)),
    Rect = {X,Y+(H-SH),W,SH},
    epx:draw_rectangle(State#state.grid,Rect),
    draw_selected_(State,Selected,Start,Stop,Zf);
draw_selected_(State,[_|Selected], Start, Stop, Zf) ->
    %% if Selection is sorted the end here!
    draw_selected_(State,Selected,Start,Stop,Zf);
draw_selected_(_State,[],_Start,_Stop,_Zf) ->
    ok.

draw_bottom_bar(State) ->
    case bar(State) of
	{_Left,_Right,_Top,0} ->
	    State;
	{_Left,_Right,_Top,Bottom} ->
	    epx_gc:set_font(State#state.font),
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(bottom_bar_color(State)), 
	    X0 = 0,
	    Y0 = State#state.height-Bottom,
	    DrawRect = {X0,Y0,State#state.width,Bottom},
	    epx:draw_rectangle(State#state.pixels, DrawRect),
	    epx_gc:set_foreground_color({0,0,0}),
	    epx_gc:set_fill_style(none),
	    epx:draw_rectangle(State#state.pixels, DrawRect),

	    %% Display Pos:  Column:  Rows: Offs: Zoom:

	    draw_text(X0+10, Y0, 100, Bottom-2,
		      "Pos: "++ integer_to_list(State#state.pos), State),
	    draw_text(X0+110, Y0, 100, Bottom-2,
		      "Columns: "++ integer_to_list(columns(State)), State),
	    draw_text(X0+210, Y0, 100, Bottom-2,
		      "Rows:"++ integer_to_list(rows(State)), State),
	    draw_text(X0+310, Y0, 130, Bottom-2, 
		      "Offs: "++ integer_to_list(State#state.noffs),State),
	    draw_text(X0+410, Y0, 130, Bottom-2, 
		      "Zoom: "++ integer_to_list(State#state.zoom),State),
	    State
    end.

draw_text(X0, Y0, _W, _H, Text, State) ->
    X = X0,
    GA = glyph_ascent(State),
    Y = Y0+1+GA,
    epx_gc:set_foreground_color(?TEXT_COLOR),
    epx:draw_string(State#state.pixels, X, Y, Text).

bottom_bar_color(State) ->
    P = State#state.profile,
    Color = P#profile.bottom_bar_color,
    epx_profile:color(P#profile.scheme, Color).

bar(#state { winfo = WI }) ->
    #window_info { left_bar = L, right_bar = R,
		   top_bar = T, bottom_bar = B } = WI,
    {L, R, T, B}.

get_base(Pos, DNA) ->
    try binary:at(DNA, Pos) of  %% range check instead?
	Base -> Base
    catch
	error:_ -> $Z
    end.

get_row(Pos, DNA, MaxLen) ->
    case DNA of
	<<_:Pos/binary, Row:MaxLen/binary, _/binary>> ->
	    Row;
	<<_:Pos/binary, Row/binary>> ->
	    Row;
	_  ->
	    <<>>
    end.


base_to_digit($T) -> $0;
base_to_digit($G) -> $1;
base_to_digit($C) -> $2;
base_to_digit($A) -> $3.

digit_to_base($0) -> $T;
digit_to_base($1) -> $G;
digit_to_base($2) -> $C;
digit_to_base($3) -> $A.

dna_to_number(DNA) when is_binary(DNA) ->
    dna_to_number(binary_to_list(DNA));
dna_to_number(DNA) when is_list(DNA) ->
    list_to_integer([base_to_digit(B)||B<-DNA], 4).

number_to_dna(Number) when is_integer(Number), Number >= 0 ->
    [digit_to_base(I) || I <- integer_to_list(Number, 4)].

string_to_dna(String) ->
    lists:append([byte_to_dna(B) || B <- String]).

byte_to_dna(B) ->
    [digit_to_base(((B bsr 6) band 3)+$0),
     digit_to_base(((B bsr 4) band 3)+$0),
     digit_to_base(((B bsr 2) band 3)+$0),
     digit_to_base((B band 3)+$0)].

%% set font that closest match the size needed
set_base_font(State) ->
    Zf = zoom_factor(State#state.zoom),
    FWidth = trunc(?BASE_WIDTH*Zf),  %% size we need to draw
    Used = find_base_font_(FWidth, State#state.font_list, undefined),
    State#state { font_used = Used }.

find_base_font_(Width, [FI=#fi{width=W}|Fis], Found) ->
    if W < Width -> 
	    find_base_font_(Width,Fis,FI);
       true ->
	    Found
    end;
find_base_font_(_Width, [], Found) ->
    Found.


glyph_width(#state { winfo = WI }) -> WI#window_info.glyph_width.
glyph_height(#state { winfo = WI }) -> WI#window_info.glyph_height.
glyph_ascent(#state { winfo = WI }) -> WI#window_info.glyph_ascent.
glyph_descent(#state { winfo = WI }) -> WI#window_info.glyph_descent.


%% convert Zoom integer -10..0..10  into zoom factor
zoom_factor(Zoom) ->
    math:pow(1.25, Zoom).

zm(W,Zf) -> max(1, W*Zf).
zm(X,Y,Zf) -> {X*Zf, Y*Zf}.

mod(A, N) ->
    if A >= 0, A < N -> A;
       true ->
	    R = A rem N,
	    if R < 0 -> R + N;
	       true -> R
	    end
    end.

scale(PointOrRect, Zoom) when Zoom == 1 -> 
    PointOrRect;
scale({X,Y}, Zoom) ->
    scale_point(X,Y,Zoom);
scale({X,Y,W,H}, Zoom) ->
    scale_rect(X,Y,W,H,Zoom).

scale_point(X,Y,Zoom) ->
    {X*Zoom, Y*Zoom}.

scale_rect(X,Y,W,H,Zoom) ->
    {X*Zoom, Y*Zoom, max(1,W*Zoom), max(1,H*Zoom)}.

%% catch up with motions
flush_motions(Window) ->
    receive
	{epx_event,Window,{motion,_,_}} ->
	    flush_motions(Window)
    after 0 ->
	    ok
    end.

flush_wheel(Window) ->
    receive
	{epx_event,Window,{_,[wheel_down],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_left],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_right],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_up],_}} ->
	    flush_wheel(Window)
    after 0 ->
	    ok
    end.
