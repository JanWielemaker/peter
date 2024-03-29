/*  $Id: login.pl,v 1.4 1999-12-08 14:35:21 jan Exp $

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(peter_login,
	  [ login/0,
	    login/1,
	    main/0,			% Toplevel runtime entry
	    save/1			% +Executable
	  ]).
:- use_module(library(pce)).
:- use_module(peter).
:- use_module(optica(configdb)).
:- use_module(optica(log)).
:- use_module(optica(util)).
:- use_module(optica(language)).
:- use_module(save).
:- use_module(rplay).

version('3.0').

		 /*******************************
		 *		SAVE		*
		 *******************************/

save(Exe) :-
	pce_image_directory(icons),
	pce_autoload_all,
	qsave_program(Exe,
		      [ goal(main),
			stand_alone(true)
		      ]).

main :-
	go_home,
	ignore(send(@pce, show_console, iconic)),
	login,
	dispatch.

go_home :-
	current_prolog_flag(saved_program, true), !,
	current_prolog_flag(resource_database, Db),
	absolute_file_name(Db, DbPath),
	file_directory_name(DbPath, BinDir),
	file_directory_name(BinDir, Home),
	chdir(Home).
go_home.

%	dispatch
%	Dispatch events as long as there is a frame for peter.  Standard
%	pce_main_loop doesn't work as Peter is not a toplevel frame.

dispatch :-
	(   catch(send(@display, dispatch), E,
		  (   term_to_atom(E, Msg),
		      send(@display, inform, Msg)
		  ))
	->  true
	;   true
	),
	get(@display?frames, find,
	    or(message(@arg1, instance_of, peter),
	       message(@arg1, instance_of, player)),
	    _Peter), !,
	dispatch.
dispatch :-
	halt.

login :-
	unix(argv(Argv)),
	login(Argv).

login(Argv) :-
	member(A, Argv),
	file_name_extension(_, cfg, A), !,
	file_directory_name(A, Dir),
	chdir(Dir),
	load_config(A),
	ignore(get(new(peter_login), confirm_centered, _)).
login(_) :-
	absolute_file_name(peter('.'),
			   [ file_type(directory),
			     access(read)
			   ],
			   PetersHome),
	chdir(PetersHome),
	load_config('file.cfg'),
	ignore(get(new(peter_login), confirm_centered, _)).

:- pce_begin_class(peter_login, dialog,
		   "Login window").

initialise(D) :->
	version(Version),
	message(file_login(Version), Msg),
	send(D, send_super, initialise, Msg),
	send(D, append, identifier_item(student, '')),
	send(D, append, new(Ok, button(ok))),
	send(D, append, button(replay)),
	send(D, append, button(cancel)),
	send(Ok, active, @off),
	send(Ok, default_button, @on).

ok(D) :->
	(   config(log(true))
	->  (   get_part(D/student, selection, Name)
	    ->  session_id(Name, Session),
		set_fact(user(Name)),
		set_fact(session(Session)),
		send(new(B, peter), open),
		start_log(B, Name, Session, _State),
		(   Session == 1
		->  send(B, show_open_texts)
		;   true
		),
		send(B, show_pre_tests)
	    ;   send(D, error, no_student_name),
		fail
	    )
	;   send(new(peter), open)
	),
	send(D, destroy).

cancel(D) :->
	send(D, destroy).

replay(D) :->
	"Activate replayer facility"::
	send(new(player), open),
	send(D, destroy).


:- pce_end_class.

%	session_id(+Name, -SessionNumber)
%
%	See which session this is.

session_id(Name, Session) :-
	config(log_directory(LogDir0)),
	absolute_file_name(LogDir0,
			   [ file_type(directory),
			     access(write)
			   ], LogDir),
	concat_atom([LogDir, /, Name], PPDir),
	new(Dir, directory(PPDir)),
	set_fact(ppdir(PPDir)),
	(   send(Dir, exists)
	->  get(Dir, files, 'session.*\\.olg', LogFileChain),
	    send(LogFileChain, sort),
	    (	get(LogFileChain, tail, F)
	    ->	file_name_extension(LogBase, _, F),
		concat(session, NumberAtom, LogBase),
		atom_chars(NumberAtom, NumberChars),
		number_chars(Number, NumberChars),
		Session is Number + 1
	    ;	Session = 1
	    )
	;   send(Dir, make),
	    Session = 1
	).


		 /*******************************
		 *	      LOG UTIL		*
		 *******************************/

:- pce_begin_class(log_window, view).

initialise(V) :->
	send(V, send_super, initialise, 'Setup log', size := size(80,20)),
	send(V, kind, popup),
	send(V, font, normal),
	send(new(D, dialog), below, V),
	send(D, append, button(ok, message(V, destroy))).

noshow_type(status).
noshow_type(progress).
noshow_type(done).

report(V, Kind:name, Fmt:[char_array], Args:any ...) :->
	(   noshow_type(Kind)
	->  true
	;   send(V, format, '[%s: ', Kind)
	),
	(   Kind == done
	->  send(V, format, 'ok\n')
	;   send(V, send_vector, format, Fmt, Args),
	    (	Kind \== progress
	    ->  send(V, format, '\n')
	    ;	true
	    )
	),
	send(V, synchronise).

log_done(V) :->
	"Done, wait for user"::
	repeat,
	    (	object(V)
	    ->	send(@display, dispatch),
		fail
	    ;	true
	    ), !.

:- pce_end_class.

start_log(Bubbles, Name, Session, State) :-
	new(Progress, log_window),
	(   config(log_show_confirm(true))
	->  send(Progress, open_centered),
	    send(Progress, wait)
	;   true
	),
	(   fact(ppdir(PPDir))
	->  new(Dir, directory(PPDir)),
	    get(Dir, files, 'session.*\\.olg', LogFileChain),
	    chain_list(LogFileChain, LogFiles0),
	    sort(LogFiles0, LogFiles),
	    free(@notes),
	    default_state(Bubbles, State0),
	    restore_logs(LogFiles, Bubbles, PPDir, Progress, State0, State),
	    fact(session(Session)),
	    get(string('%s/session%03d.olg', PPDir, Session), value, LogFile),
	    send(Progress, report, progress,
		 'Opening new log file %s ...', LogFile),
	    close_log,
	    open_log(LogFile, 'FILE', Name),
	    send(Progress, report, done)
	;   true
	),
	send(Progress, report, status,
	     'Initialisation successful'),
	(   config(log_show_confirm(true))
	->  send(Progress, log_done)
	;   send(Progress, destroy)
	).

default_state(_, state([], [], @nil)).

restore_logs([], _, _, _, State, State).
restore_logs([H|T], Bubbles, Dir, Progress, State0, State) :-
	restore_log(H, Bubbles, Dir, Progress, State0, State1),
	restore_logs(T, Bubbles, Dir, Progress, State1, State).

restore_log(F, Bubbles, PPDir, Progress, State0, State) :-
	send(Progress, report, progress, 'Reading log file %s ... ', F),
	concat_atom([PPDir, /, F], OldLog),
	read_log(Bubbles, OldLog, State0, State),
	send(Progress, report, done).
