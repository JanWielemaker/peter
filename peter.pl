/*  $Id: peter.pl,v 1.4 1999-12-08 14:35:21 jan Exp $

    Part of Peter
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(peter,
	  [ set_icon_path/0
	  ]).
:- use_module(library(pce)).

		 /*******************************
		 *	       PATHS		*
		 *******************************/

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(optica, '../optica').
user:file_search_path(local, '.').
user:file_search_path(peter, swi(peter)) :-
	absolute_file_name(swi(peter),
			   [ file_type(directory),
			     access(read),
			     file_errors(fail)
			   ], _), !.
user:file_search_path(peter, '.').

:- use_module(optica(configdb)).

set_icon_path :-
	pce_image_directory(peter('icons')),
	(   config(icon_directory(Dir))
	->  pce_image_directory(Dir)
	;   true
	).

		 /*******************************
		 *	      LOADING		*
		 *******************************/

:- use_module(select).
:- use_module(experiment).
:- use_module(optica(configdb)).
:- use_module(optica(language)).
:- use_module(optica(text)).
:- use_module(optica(log)).
:- use_module(messages).
:- use_module(getpass).

		 /*******************************
		 *	     OPERATORS		*
		 *******************************/

:- initialization op(1, fx, user:($)).


		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

:- pce_begin_class(peter, frame,
		   "Peter task main-window").

variable(started,	date*,	get, "Start of experiment").

initialise(P) :->
	set_icon_path,
	config(title(Title)),
	send(P, send_super, initialise, Title),
	send(P, done_message, message(P, quit)),
	(   config(window_style(Style))
	->  send(P, kind, Style)
	;   true
	),
	send(P, append, new(D, dialog)),
	new(SW, select_window),
	(   config(result_window_placement(Placement))
	->  true
	;   Placement = below
	),
	send(new(EW, experiment_window), Placement, SW),
	send(EW, below, D),
	send(P, fill_dialog),
	send(EW, new_experiment),
	send(SW, activate_buttons).

open(Peter) :->
	"Open according to settings in the config file"::
	(   config(window_status(Status)),
	    Status \== window
	->  send(Peter, status, Status)
	;   send(Peter, open_centered)
	).

fill_dialog(P) :->
	get(P, member, dialog, D),
	send(D, gap, size(0, 5)),
	send(D, pen, 0),
	(   config(button(Cmd, Label, Summary)),
	    Cmd =.. [Name|Args],
	    Msg =.. [message, P, Name | Args],
	    send(D, append, new(B, button(Cmd, Msg))),
	    send(B, label, Label),
	    send(B, help_message, tag, Summary),
	    fail
	;   true
	),
	send(D, append, graphical(0, 0, 10, 1), right),
	(   config(text(Name, Label, _File, _Editable)),
	    Label \== @nil,
	    send(D, append,
		 new(B, button(Name, message(P, show_text, Name))), right),
	    send(B, label, Label),
	    fail
	;   true
	).

:- pce_group(button_action).

show_selection(P) :->
	"Show currently selection, experiments"::
	get(P, experiments, EW),
	send(EW, show_selection).

show_open_texts(P) :->
	"Show welcome/theory texts"::
	send(P, wait),
	send(timer(0.5), delay),
	(   config(pre_text(Text)),
	    send(P, show_text, Text),
	    fail
	;   true
	).

show_pre_tests(_P) :->
	true.

show_text(P, Name:name) :->
	"Show named text"::
	config(text(Name, _Label, File, Editable)),
	new(T, textviewer(Name, task, @default, Editable, File)),
	send(T, transient_for, P),
	send(T, modal, transient),
	log(begin(text(Name))),
	ignore(get(T, confirm_centered, P?area?center, _)),
	log(end).

quit(P) :->
	message(quit, Label),
	new(D, dialog(Label)),
	send(D, transient_for, P),
	send(D, modal, transient),
	(   config(quit_text(Text))
	->  true
	;   new(Text, string('Quit %s', P?label))
	),
	send(D, append, label(info, Text)),
	(   config(quit_password(Passwd))
	->  send(D, append, new(Pwd, passwd_item))
	;   true
	),
	send(D, append, button(quit, message(D, return, quit))),
	send(D, append, button(cancel, message(D, return, cancel))),
	send(D, default_button, quit),
	between(0, 5, _),
	object(D),
	get(D, confirm_centered, P?area?center, Rval),
	(   Rval == cancel
	->  !,
	    send(D, destroy),
	    fail
	;   (   object(Pwd),
	        get(Pwd, selection, PasswdString),
		\+ get(PasswdString, value, Passwd)
	    ->  message(invalid_password, Msg),
		send(P?display, inform, Msg),
		send(Pwd, clear),
		fail
	    ;   !
	    ),
	    send(P, destroy)
	).


finish(P) :->
	"User indicates s/he is finished"::
	message(finished, Label),
	new(D, dialog(Label)),
	send(D, transient_for, P),
	send(D, modal, transient),
	(   config(finish_text(Text))
	->  true
	;   Text = 'I''m finished'
	),
	send(D, append, label(info, Text)),
	(   config(finish_password(Passwd))
	->  send(D, append, new(Pwd, passwd_item))
	;   true
	),
	(   config(finish_allow_for_quit(true))
	->  send(D, append, button(quit_peter, message(D, return, quit)))
	;   true
	),
	send(D, append, button(disable_experiments, message(D, return, ok))),
	send(D, append, button(cancel_finish, message(D, return, cancel))),
	send(D, default_button, disable_experiments),
	between(0, 5, _),
	object(D),
	get(D, confirm_centered, P?area?center, Rval),
	(   Rval == cancel
	->  !,
	    send(D, destroy),
	    fail
	;   (   object(Pwd),
	        get(Pwd, selection, PasswdString),
		\+ get(PasswdString, value, Passwd)
	    ->  message(invalid_password, Msg),
		send(P?display, inform, Msg),
		send(Pwd, clear),
		fail
	    ;   !
	    ),
	    send(D, destroy),
	    (   Rval == quit
	    ->  send(P, destroy)
	    ;   send(P, do_finish)
	    )
	).

do_finish(P) :->
	"Disable further experimentation"::
	get(P, member, select_window, W),
	send(W, active, @off).

:- pce_group(external_control).

experiments(P, EW:experiment_window) :<-
	get(P, member, experiment_window, EW).

selected_variable(P, Name:name, Value:any) :->
	"PP has selected a variable"::
	get(P, member, experiment_window, EW),
	send(EW, selected_variable, Name, Value).

:- pce_group(replay).

set_variable(P, Name:name, Value:any) :->
	"Set variable to value"::
	get(P, member, select_window, SW),
	send(SW, selected_variable, Name, Value).

take_back(P, _Variable:[name]) :->
	"Take back the last selected variable"::
	get(P, member, select_window, SW),
	send(SW, take_back).

result(P, Prediction:[any]) :->
	"Show the result"::
	get(P, member, select_window, SW),
	(   Prediction \== @default
	->  get(SW, member, prediction, PI),
	    send(PI, selection, Prediction)
	;   true
	),
	send(SW, result).

reinitialise(P) :->
	"Clear the Peter task"::
	get(P, member, select_window, SW),
	get(P, experiments, EC),
	send(SW, reinitialise),
	send(EC, reinitialise),
	send(EC, new_experiment).

scroll_to(P, Start:int) :->
	"Scroll to indicated experiment"::
	get(P, experiments, EW),
	send(EW, start, Start).

experiment(P, N:int, E:experiment) :<-
	"Find numbered experiment"::
	get(P, experiments, EW),
	get(EW, experiments, Chain),
	get(@pce, convert, N, name, Name),
	get(Chain, find, @arg1?name == Name, E).

select_experiment(P, N, Op:{add,delete}) :->
	"(De)Select numbered experiment"::
	get(P, experiment, N, E),
	(   Op == add
	->  send(E, selected, @on)
	;   send(E, selected, @off)
	).

clear_selection(P) :->
	"Deselect all experiments"::
	get(P, experiments, EW),
	send(EW, clear_selection).

open_selection(P, Selection:chain, Frame:frame) :<-
	"Open selected objects and return window"::
	get(P, experiments, EW),
	get(Selection, map, ?(P, experiment, @arg1), Objects),
	get(EW, open_selection, Objects, Frame).


up(P, N:int) :->
	"Move experiment `up'"::
	get(P, experiment, N, E),
	send(E, up).


down(P, N:int) :->
	"Move experiment `down'"::
	get(P, experiment, N, E),
	send(E, down).

:- pce_group(time).

start(P) :->
	"Experimenting has started"::
	(   get(P, started, Date),
	    Date \== @nil
	->  true
	;   send(P, slot, started, new(date))
	).

check_time_limit(P) :->
	"Check whether time has been exceeded"::
	(   config(time_limit(Time))
	->  (   get(P, started, Start),
	        get(new(date), difference, Start, second, S),
		S > Time * 60
	    ->	send(P, time_limit_exceeded),
		fail
	    ;	true
	    )
	;   true
	).

time_limit_exceeded(P) :->
	"Disable further experiments and optionally show text"::
	send(P, do_finish),
	(   config(time_exceeded_text(Text))
	->  send(P, show_text, Text)
	;   true
	).

:- pce_end_class.


