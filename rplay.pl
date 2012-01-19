/*  $Id: rplay.pl,v 1.4 1999/12/08 16:04:06 jan Exp $

    Part of Optica
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(rplay,
	  [ rplay/0
	  ]).
:- use_module(save).
:- use_module(optica(pretty_print)).
:- use_module(optica(configdb)).
:- use_module(library(toolbar)).
:- use_module(optica(hourglass)).
:- use_module(optica(parts)).
:- use_module(peter).			% set_icon_path/0
:- use_module(experiment).		% compute_result/2

:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).

resource(open,	      image, image('16x16/open.xpm')).
resource(text,	      image, image('text.xpm')).
resource(undo,	      image, image('undo.xpm')).
resource(ok,	      image, image('answer_good.xpm')).
resource(wrong,	      image, image('answer_bad.xpm')).
resource(scroll,      image, image('scroll.xpm')).
resource(select,      image, image('select.xpm')).
resource(deselect,    image, image('deselect.xpm')).
resource(deselectall, image, image('deselectall.xpm')).
resource(up,	      image, image('up.xpm')).
resource(down,	      image, image('down.xpm')).
resource(magnify,     image, image('magnify.xpm')).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Replay Peter task logfiles.  Design:

	* Load a logfile, storing it as the predicates

		log_header(HeaderTerm).
	  	log_event(N, Time, Event).

	  Multiple logfiles can be concatenated this way.

	* Create a `player'.  This displays general info, a browser holding
	  the events, settings for speed, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      DATABASE		*
		 *******************************/

:- dynamic
	log_event/3,
	log_header/1.

clear_log_database :-
	retractall(log_event(_,_,_)),
	retractall(log_header(_)),
	flag(log_last, _, 1).

%	log_end_time(-Time)
%
%	Find the end of the current log

log_end_time(Time) :-
	flag(log_last, N, N),
	Last is N - 1,
	log_event(Last, Stamp, _Event), !,
	(   Stamp = From-To
	->  Time = To
	;   Time = From
	).
log_end_time(0).

add_time_stamp(Time, 0, Time) :- !.
add_time_stamp(From0-To0, Offset, From-To) :- !,
	From is From0 + Offset,
	To is To0 + Offset.
add_time_stamp(Time0, Offset, Time) :- !,
	Time is Time0 + Offset.

%	time_of_event(+Id, -StartTime)

time_of_event(Id, Time) :-
	log_event(Id, Stamp, _Event),
	(   Stamp = From-_To
	->  Time = From
	;   Time = Stamp
	).

%	log_event_id(Id)
%	
%	Enumaret the event-id's of the current log.

log_event_id(Id) :-
	flag(log_last, N, N),
	Last is N-1,
	between(1, Last, Id).

%	event_before(+Id0, -Id)
%
%	Generate events before this one

event_before(Id, Id).
event_before(1, _) :- !,
	fail.
event_before(Id0, Id) :-
	Id1 is Id0 - 1,
	event_before(Id1, Id).

%	experiment_open_event(+Id, -ExperimentOpenId)
%
%

experiment_open_event(Id, ExperimentOpenId) :-
	event_before(Id, ExperimentOpenId),
	log_event(ExperimentOpenId, _Time, experiment(open(_,_))), !.

%	read_logfile(+File)
%
%	Read facts from the given logfile and assert them into the database.

read_logfile(File) :-
	open(File, read, Fd),
	log_end_time(Offset),
	read(Fd, Term),
	read_logfile(Term, Fd, Offset),
	close(Fd).

read_logfile(end_of_file, _, _) :- !.
read_logfile(@(Time0, Event0), Fd, Offset) :- !,
	flag(log_last, N, N+1),
	add_time_stamp(Time0, Offset, Time),
	fix_event(Event0, Event),
	assert(log_event(N, Time, Event)),
	read(Fd, Next),
	read_logfile(Next, Fd, Offset).
read_logfile(Term, Fd, Offset) :-
	assert(log_header(Term)),
	read(Fd, Next),
	read_logfile(Next, Fd, Offset).

fix_event(result(Bindings, @_, Prediction),
	  result(Bindings, Answer, Prediction)) :- !,
	compute_result(Bindings, Answer).
fix_event(Event, Event).


		 /*******************************
		 *	      SHOW_LOG		*
		 *******************************/

:- pce_begin_class(log_browser, browser,
		   "Browse log events").

variable(experiment,	name*,	get, "Current experiment id").
variable(current,	int*,   get, "Current location").

initialise(LB) :->
	send(LB, send_super, initialise, @default, size(33, 20)),
	send(LB?image, tab_stops, vector(130)),
	send(LB, select_message,
	     message(LB?frame, select_event, @arg1?key, @on)),
	send(LB, open_message,
	     message(LB?frame, show_status_at, @arg1?key)),
	send(LB, update).


enter(LB) :->
	get(LB, frame, Player),
	(   get(Player, play_status, idle)
	->  (   get(LB?list_browser, slot, search_string, SS),
	        SS \== @nil
	    ->  send(LB, send_super, enter)
	    ;   get(LB, selection, DI),
		get(DI, key, Id0),
		(   get(Player, current_id, Id0)
		->  Id is Id0+1,
		    time_of_event(Id, Time0),
		    send(Player, select_event, Id, @on),
		    get(LB, member, Id, DI2),
		    send(LB, normalise, DI2),
		    get_time(Epoch),
		    send(Player, slot, epoch, Epoch),
		    send(Player, slot, time_skipped, Time0),
		    send(Player, slot, skipping, @off),
		    play_to(Player, Id)
		;   send(Player, show_status_at, Id0)
		)
	    )
	;   send(Player, schedule, message(LB, enter))
	).


update(LB) :->
	"Reload a the log facts"::
	send(LB, current, @nil),
	send(LB, clear),
	(   log_event_id(Id),
		(   log_event(Id, Stamp, Event),
		    event_icon(Event, Icon, Comment),
		    icon_name(Icon, IconName)
		->  send(LB, register_icon, IconName, Icon),
		    event_label(Stamp, Comment, Label),
		    send(LB, append, dict_item(Id, Label, @nil, IconName))
		),
	    fail
	;   true
	).


icon_name(resource(X), X) :- !.
icon_name(X, X).


current(LB, Id:[int*]) :->
	"Set current and provide feedback"::
	(   get(LB, current, Old),
	    Old \== @nil,
	    get(LB, member, Old, OldDI),
	    get(OldDI, style, OldStyle),
	    atom_concat(current_, PlainStyle, OldStyle),
	    send(OldDI, style, PlainStyle)
	;   true
	),
	send(LB, slot, current, Id),
	(   Id == @nil
	->  true
	;   get(LB, member, Id, DI),
	    get(DI, style, PlainStyle),
	    atom_concat(current_, PlainStyle, NewStyle),
	    send(DI, style, NewStyle)
	).


register_icon(LB, IconName:name, Icon:'name|resource') :->
	(   get(LB, styles, StyleSheet),
	    get(StyleSheet, value, IconName, _)
	->  true
	;   new(I, image(Icon))
	->  (	get(I, size, size(16,16))
	    ->	Img = I
	    ;	get(I, scale, size(16,16), Img),
		send(Img, lock_object, @on)
	    ),
	    send(LB, style, IconName, style(Img)),
	    atom_concat(current_, IconName, CurrentStyle),
	    send(LB, style, CurrentStyle,
		 style(Img, background := light_sky_blue))
	;   send(LB, style, IconName, new(style)),
	    atom_concat(current_, IconName, CurrentStyle),
	    send(LB, style, CurrentStyle,
		 style(background := light_sky_blue))
	).

event_label(From-To, Comment, String) :- !,
	MinF is round(From)//60,
	SecF is	round(From) mod 60,
	MinT is round(To)//60,
	SecT is	round(To) mod 60,
	new(String, string(' [%02d:%02d-%02d:%02d]\t%s',
			   MinF, SecF, MinT, SecT, Comment)).
event_label(Time, Comment, String) :-
	Min is round(Time)//60,
	Sec is	round(Time) mod 60,
	new(String, string(' [%02d:%02d]\t%s', Min, Sec, Comment)).

%	event_icon(+Event, -Icon, -Label).

event_icon(text(Text, _), Icon,	Text) :-
	config(text(Text, Icon, _File, _Editable)),
	Icon \== @nil, !.
event_icon(text(Text, _), resource(text), Text).
event_icon(set(Variable, Value), Icon, '') :-
	config(variable(Variable, Values)),
	memberchk(Value = Icon, Values).
event_icon(unset(Variable), resource(undo), Variable).
event_icon(result(_, P, prediction(P)), resource(ok), P) :- !.
event_icon(result(_, A, prediction(P)), resource(wrong), Label) :-
	sformat(Label, '~w (ok=~w)', [P, A]).
event_icon(scroll(Visible), resource(scroll), Label) :-
	concat_atom(Visible, ' ', Label).
event_icon(selected(N),     resource(select), N).
event_icon(deselected(N),   resource(deselect), N).
event_icon(clear_selection, resource(deselectall), '').
event_icon(show_selection(_Set, _Scroll), resource(magnify), '').
event_icon(up(N),	    resource(up), N).
event_icon(down(N),	    resource(down), N).
event_icon(Term, _, _) :-
	format('Warning: no event-icon for ~w~n', [Term]),
	fail.

:- pce_end_class.

		 /*******************************
		 *	       PLAYER		*
		 *******************************/

:- pce_begin_class(player, frame,
		   "Replayer of Peter logfiles").


variable(current_id,	int*,		get, "Currently simulated Id").
variable(play_status,	{idle,playing} := idle, get, "Status").
variable(epoch,		real*,		get, "Wall-clock for T=0").
variable(time_skipped,	real*,		get, "Amount of time skipped").
variable(time_scale,	real,		get, "Time scaling").
variable(pauze_limit,	real,		get, "Max pause").
variable(skipping,	bool := @off,	get, "Don't show states").
variable(msg_queue,	chain,		get, "Queued messages").

initialise(P) :->
	set_icon_path,
	retractall(config_db:config(beep_on_result(_))),
	retractall(config_db:config(finish_password(_))),
	retractall(config_db:config(quit_password(_))),

	send(P, send_super, initialise, 'FILE Logfile Player'),
	send(P, slot, time_scale, 1),
	send(P, slot, pauze_limit, 3),
	send(P, slot, msg_queue, new(chain)),
	send(P, append, new(D, dialog)),
	send(D, append, new(TB, tool_bar(P))),
	send(D, append, new(HG, hourglass), right),
	send(D, gap, size(0, 5)),
	send(HG, alignment, right),
	send(TB, attribute, reference, point(0,0)),
	send(HG, attribute, reference, point(0,0)),
	send(D, resize_message, message(D, layout, @arg2)),
	send_list(TB, append, 
		  [ tool_button(load,
				resource(open),
				'Load Logfile')
		  ]),
	send(new(V, view), right, new(log_browser)),
	send(V, size, size(50, 10)),
	send(V, below, D).

schedule(P, Msg:code) :->
	send(P?msg_queue, append, Msg),
	get(P, member, dialog, Dialog),
	get(Dialog, member, hourglass, HG),
	send(HG, abort),
	send(P, slot, skipping, @on).

load(P) :->
	"Load a logfile"::
	get(@finder, file, @on, olg, File),
	clear_log_database,
	read_logfile(File),
	send(P, update).

update(P) :->
	get(P, member, log_browser, LB),
	send(LB, update).

select_event(P, Id:int, ShowDetails:[bool]) :->
	"Show event of given id"::
	get(P, member, log_browser, LB),
	send(LB, current, Id),
	send(LB, selection, Id),
	(   ShowDetails \== @off
	->  get(P, member, view, View),
	    send(View, clear),
	    log_event(Id, _Time, Term),
	    pce_open(View, write, Fd),
	    pretty_print(Fd, Term),
	    close(Fd),
	    send(View, caret, 0),
	    send(View, scroll_to, 0),
	    send(View, editable, @off)
	;   true
	).

:- pce_group(simulator).

simulator(P, Appl:frame) :<-
	"Get an instance of the simulator suitable for playing"::
	(   get(P, hypered, simulator, Appl)
	->  true
	;   send(P, slot, current_id, 0),
	    new(Appl, peter),
	    send(Appl, open),
	    new(_, hyper(P, Appl, simulator, player))
	).

show_status_at(P, Id:int) :->
	"Show status at indicated location"::
	(   get(P, current_id, Current),
	    integer(Current),
	    Id >= Current
	->  true
	;   get(P, simulator, Peter),
	    send(Peter, reinitialise),
	    send(P, slot, current_id, 0)
	),
	send(P, slot, skipping, @on),
	play_to(P, Id),
	get(P, member, log_browser, LB),
	send(LB, current, Id).


play_to(P, Id:int) :->
	"Simulate upto the given event-id"::
	send(P, goto_start_of_experiment, Id),
	send(P, slot, skipping, @off),
	play_to(P, Id).
	

play_to(P, Id) :-
	send(P, slot, play_status, playing),
	do_play_to(P, Id),
	send(P, slot, play_status, idle),
	get(P, msg_queue, Queue),
	repeat,
	    (	send(Queue, empty)
	    ->	!
	    ;	get(Queue, delete_head, Msg),
		send(Msg, forward, P),
		fail
	    ).


do_play_to(P, Id) :-
	get(P, current_id, Current),
	Current >= Id, !.
do_play_to(P, Id) :-
	get(P, current_id, CurrentId),
	NextId is CurrentId+1,
	send(P, slot, current_id, NextId),
	send(P, select_event, NextId, @off),
	log_event(NextId, Time, Term),
	rplay(Time, Term, P),
	(   get(P, skipping, @on)
	->  true
	;   send(P, synchronise)
	),
	do_play_to(P, Id).


delay_to(P, Time:real) :->
	"Wait upto the specified time"::
	(   get(P, skipping, @on)
	->  true
	;   get(P, epoch, Epoch),
	    get(P, time_skipped, Skipped),
	    get(P, time_scale, Scale),
	    get_time(Now),
	    RealDelay is Epoch+Time-Skipped-Now,
	    Delay is RealDelay * Scale,
	    (   Delay =< 0.01
	    ->  true
	    ;   get(P, pauze_limit, Limit),
		Delay > Limit
	    ->  NewSkip is Skipped + (Delay-Limit)/Scale,
		send(P, slot, time_skipped, NewSkip),
		send(P, delay, Limit)
	    ;   send(P, delay, Delay)
	    )
	).
	
delay(P, Time:real) :->
	"Do the real delay"::
	get(P, member, dialog, D),
	get(D, member, hourglass, HG),
	send(HG, displayed, @on),
	Initial is max(2, min(9, integer(Time/0.3))),
	send(HG, wait, Time, Initial),
	send(HG, displayed, @off).

:- pce_end_class.

rplay :-
	load_config('peter.cfg'),
	send(new(player), open).


		 /*******************************
		 *	    SIMULATING		*
		 *******************************/


rplay(From-To, text(Text, _), Player) :- !,
	(   get(Player, skipping, @on)
	->  true
	;   send(Player, delay_to, From),
	    get(Player, simulator, Optica),
	    send(Optica, text_viewer, Text, @off),
	    send(Player, delay_to, To),
	    get(Optica, transients, Frames),
	    get(Frames, find, message(@arg1, instance_of, textviewer), TV),
	    send(TV, show, @off),		% Why do we need this?
	    send(TV, destroy)
	).
rplay(From-To, show_selection(List, _Scroll), Player) :- !,
	(   get(Player, skipping, @on)
	->  true
	;   send(Player, delay_to, From),
	    get(Player, simulator, Peter),
	    chain_list(Chain, List),
	    get(Peter, open_selection, Chain, Frame),
	    send(Player, delay_to, To),
	    send(Frame, free),
	    get(Peter, experiments, EW), % HACK
	    send(EW, reposition)
	).
rplay(From-_To, Action, Player) :- !,
	send(Player, delay_to, From),
	get(Player, simulator, Optica),
	rdo(Action, Optica).
rplay(Time, Action, Player) :-
	send(Player, delay_to, Time),
	get(Player, simulator, Optica),
	rdo(Action, Optica).
	
rdo(set(Variable, Value), Peter) :-
	send(Peter, set_variable, Variable, Value).
rdo(unset(_Variable), Peter) :-
	send(Peter, take_back).
rdo(result(_Bindings, _Answer, prediction(Predication)), Peter) :-
	send(Peter, result, Predication).
rdo(result(_Bindings, _Answer), Peter) :-
	send(Peter, result).
rdo(scroll([V0|_]), Peter) :-
	send(Peter, scroll_to, V0).
rdo(selected(N), Peter) :-
	send(Peter, select_experiment, N, add).
rdo(deselected(N), Peter) :-
	send(Peter, select_experiment, N, delete).
rdo(up(N), Peter) :-
	send(Peter, up, N).
rdo(down(N), Peter) :-
	send(Peter, down, N).
rdo(clear_selection, Peter) :-
	send(Peter, clear_selection).
