/*  $Id: mkruntime.pl,v 1.2 1999/12/08 14:35:21 jan Exp $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(exe,
	  [ make_runtime/0		% Build runtime version
	  ]).

		 /*******************************
		 *	     SPECS		*
		 *******************************/

:- dynamic
	release/0.

user:file_search_path(dll, Dir) :-
	current_prolog_flag(executable, WinExe),
	prolog_to_os_filename(Exe, WinExe),
	file_directory_name(Exe, Dir).

runtime_dir(runtime).			% path to runtime directory
runtime_exe('peter.exe').

bindir(runtime(bin)).

option(emulator(dll('swipl-win.exe'))).
option(goal(main)).

resource('*.cfg').
resource(icons).
resource(pictures).
resource('ChangeLog').
resource('*.html').

dll('libswipl.dll').
dll('pl2xpce.dll').
dll('plterm.dll').
dll('libgcc_s_seh-1.dll').
dll('libwinpthread-1.dll').
dll('zlib1.dll').
dll('libgmp-10.dll').

ignore_file('CVS').
ignore_file('Backup').
ignore_file('*~').
ignore_file('*.old').
ignore_file('*.bak').
ignore_file('#*').

directory(runtime(.)).
directory(runtime(log)).
directory(Bin) :-
	bindir(Bin).

:- pce_image_directory(icons).

		 /*******************************
		 *	   SEARCH PATHS		*
		 *******************************/

%user:file_search_path(swi,     '/staff/jan/einstein/src/pl').
user:file_search_path(runtime, Dir) :-
	runtime_dir(Dir).
user:file_search_path(bindir, Dir) :-
	bindir(Dir).


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

make_runtime :-
	make_directories,
	make_swipl,
	copy_dlls,
	copy_resources,
	pce_autoload_all,
	findall(O, option(O), Options),
	runtime_exe(Exe),
	absolute_file_name(bindir(Exe), ExePath),
	qsave_program(ExePath, Options).

make_swipl :-
	absolute_file_name(bindir(..), OneUp),
	absolute_file_name(runtime(.), RunTime),
	(   OneUp == RunTime
	->  SwiPl = '.'
	),
	atom_concat(OneUp, '/swipl', File),
	open(File, write, Fd),
	format(Fd, '~w~n', SwiPl),
	close(Fd).
make_swipl :-
	format('Warning: Cannot determine contents for swipl~n', []).


%	make_directories
%
%	Create all required directories

make_directories :-
	forall(directory(Spec),
	       (absolute_file_name(Spec, Dir),
		ensure_directory(Dir))).

%%	ensure_directory(+Dir)
%
%	Ensure the existence of directory Dir

ensure_directory(Dir) :-
	exists_directory(Dir), !.
ensure_directory(Dir) :-
	make_directory(Dir).

copy_resources :-
	forall(resource(Spec), copy_resource(Spec, [])),
	forall(resource(Spec, Options), copy_resource(Spec, Options)).

copy_resource(Spec, Options) :-
	atomic(Spec), !,
	dest_dir(Options, ToDir),
	expand_file_name(Spec, Matches),
	forall(member(Match, Matches), make(Match, ToDir)).
copy_resource(Spec, Options) :-
	dest_dir(Options, ToDir),
	absolute_file_name(Spec,
			   [ access(read)
			   ], Path),
	make(Path, ToDir).

copy_dlls :-
	forall(dll(Spec), copy_resource(dll(Spec), [type(dll)])).

dest_dir(Options, Dir) :-
	memberchk(type(dll), Options), !,
	absolute_file_name(bindir('.'), Dir).
dest_dir(_Options, Dir) :-
	absolute_file_name(runtime('.'), Dir).


%	Copy a target if it was modified.

make(Path, _To) :-
	file_base_name(Path, Base),
	ignore_file(Ignore),
	wildcard_match(Ignore, Base), !.
make(Path, To) :-
	exists_directory(Path), !,
	make_directory(Path, To).
make(Path, To) :-
	make_file(Path, To).

make_file(Path, To) :-
	file_base_name(Path, Base),
	atomic_list_concat([To, Base], /, ToFile),
	(   exists_file(ToFile),
	    time_file(Path, FromTime),
	    time_file(ToFile, ToTime),
	    ToTime >= FromTime
	->  true
	;   feedback('Copying ~w --> ~w ... ', [Path, ToFile]),
	    open(Path, read, In, [type(binary)]),
	    open(ToFile, write, Out, [type(binary)]),
	    copy_stream_data(In, Out),
	    close(In),
	    close(Out),
	    feedback('ok~n', [])
	).

make_directory(Source, Dest) :-
	file_base_name(Source, Base),
	concat_atom([Dest, Base], /, ToDir),
	ensure_directory(ToDir),
	atom_concat(Source, '/*', Pattern),
	expand_file_name(Pattern, Matches),
	forall(member(Match, Matches), make(Match, ToDir)).

		 /*******************************
		 *	     FEEDBACK		*
		 *******************************/

feedback(Fmt, Args) :-
	format(user_error, Fmt, Args).
