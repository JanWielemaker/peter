/*  $Id: select.pl,v 1.4 1999-12-08 14:35:21 jan Exp $

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(select, []).
:- use_module(library(pce)).
:- use_module(optica(configdb)).
:- use_module(optica(log)).

:- pce_begin_class(select_window, dialog,
		   "Window for selecting variable values").

initialise(SW) :->
	send(SW, send_super, initialise),
	send(SW, ver_stretch, 100),
	(   config(stretch_select_window(true))
	->  send(SW, resize_message, message(SW, layout, @arg2))
	;   true
	),
	(   config(variable(Name, _)),
	    send(SW, append,
		 select_variable(Name, message(SW, selected_variable, 
					       Name, @arg1))),
	    fail
	;   true
	),
	(   config(ask_prediction(true))
	->  send(SW, append,
		 new(PI, predictor_item(prediction))),
	    send(PI, clear)		% no default value
	;   true
	),
	send(SW, append, button(take_back)),
	send(SW, append, new(RB, button(result))),
	send(RB, default_button, @on).
		   
selected_variable(SW, Name:name, Value:any) :->
	"PP selected value for variable"::
	get(SW, frame, Peter),
	send(Peter, start),		% start the clock if not done
	get(Peter, experiments, EC),
	get(Peter, member, experiment_window, EW),
	get(EC, current, E),
	send(EW, normalise, E),
	send(E, selected_variable, Name, Value),
	get(SW, member, Name, Menu),
	send(Menu, active, @off),
	send(SW, activate_buttons),
	log(set(Name, Value)).

reinitialise(SW) :->
	"Prepare for new experiment"::
	send(SW?graphicals, for_all,
	     if(message(@arg1, has_send_method, reinitialise),
		message(@arg1, reinitialise))),
	send(SW, activate_buttons).

result(SW) :->
	"Compute the result"::
	(   config(beep_on_result(Volume))
	->  send(SW, bell, Volume)
	;   true
	),
	get(SW?frame, experiments, EC),
	get(EC, current, E),
	(   get(SW, member, prediction, PI),
	    get(PI, selection, Prediction)
	->  send(E, prediction, Prediction)
	;   true
	),
	send(E, compute_result),
	experiment_result(E, Bindings),
	get(E, answer, Answer),
	prolog_answer(Answer, Prolog),
	(   var(Prediction)
	->  log(result(Bindings, Prolog))
	;   log(result(Bindings, Prolog, prediction(Prediction)))
	),
	(   send(SW?frame, check_time_limit)
	->  send(EC, new_experiment),
	    send(SW, reinitialise)
	;   true
	).
	
prolog_answer(@String, Prolog) :- !,
	get(@String, value, Atom),
	prolog_answer(Atom, Prolog).
prolog_answer(Raw, Pl) :-
	name(Raw, Chars),
	name(Pl, Chars).

experiment_result(E, Bindings) :-
	get_chain(E, graphicals, Grs),
	values_from_graphicals(Grs, Bindings).

values_from_graphicals([], []).
values_from_graphicals([H|T], [Term|R]) :-
	send(H, instance_of, experiment_cell),
	get(H, name, Name),
	get(H, value, Value),
	Value \== @nil, !,
	Term =.. [Name, Value],
	values_from_graphicals(T, R).
values_from_graphicals([_|T], V) :-
	values_from_graphicals(T, V).

take_back(SW) :->
	"Take back last one"::
	get(SW?frame?experiments, current, E),
	get(E, take_back, VarName),
	get(SW, member, VarName, Menu),
	send(Menu, reinitialise),
	send(SW, activate_buttons),
	log(unset(VarName)).

%	activate(+Status, +PIStat, -TakeBack, -Result, -PIActive)

activate(empty,   _,      @off, @off, @off).
activate(filled,  filled, @on,  @on,  @on).
activate(filled,  empty,  @on,  @off,  @on).
activate(filling, _,      @on,  @off, @off).

activate_buttons(SW) :->
	"(De)activate the result/take_back buttons"::
	get(SW, member, result, ResultButton),
	get(SW, member, take_back, TakeBackButton),
	(   get(SW, member, prediction, PredictionItem)
	->  get(PredictionItem, status, PIStat)
	;   true
	),
	get(SW?frame?experiments, current, E),
	get(E, status, Status),
	activate(Status, PIStat, RA, TA, PIA),
	send(TakeBackButton, active, RA),
	send(ResultButton, active, TA),
	(   nonvar(PredictionItem)
	->  send(PredictionItem, active, PIA)
	;   true
	).

modified_item(_SW, _Item:graphical, _Modified:bool) :->
	fail.

active(SW, Val:bool) :->
	"(De)activate the whole experiment window"::
	send(SW, send_super, active, Val),
	send(SW?graphicals, for_all,
	     if(message(@arg1, instance_of, select_variable),
		message(@arg1, active, Val))).

:- pce_end_class.


:- pce_begin_class(select_variable, menu,
		   "Select a variable value").

initialise(VM, Name:name, Msg:[code]*) :->
	send(VM, send_super, initialise, Name, choice, Msg),
	send(VM, multiple_selection, @on),
	send(VM, layout, horizontal),
	send(VM, alignment, center),
	send(VM, show_label, @off),
	config(variable(Name, Values)),
	(   member(ValName=Image, Values),
	    send(VM, append, menu_item(ValName, @default, image(Image))),
	    fail
	;   true
	).

reinitialise(VM) :->
	"Prepare for new experiment"::
	send(VM, active, @on),
	send(VM, selection, @nil).

active(VM, Val:bool) :->
	(   get(VM, active, Val)
	->  true
	;   send(VM, send_super, active, Val),
	    (   Val == @off
	    ->  get(VM, area, area(X,Y,W,H)),
		new(BM, bitmap(new(I, image(@nil, W, H)), @on)),
		send(I, fill, @grey50_image),
		send(VM?device, display, BM, point(X, Y)),
		send(VM, attribute, greymask, BM)
	    ;	(   get(VM, attribute, greymask, BM)
		->  send(VM, delete_attribute, greymask),
		    free(BM)
		;   true
		)
	    )
	).

:- pce_end_class.

:- pce_begin_class(predictor_item, text_item,
		   "Show/edit user prediction").

class_variable(border,		 '0..',	10,    "Border around text").
class_variable(value_font,	 font,	large, "Font used for value").
class_variable(length,		 int,	5,     "# characters displayed").
class_variable(combo_box_heigth, '1..',	20,    "Max heigth of shown ComboBox").

initialise(I, Name:[name]) :->
	send_super(I, initialise, Name),
	(   config(value_set(List))
	->  chain_list(VS, List),
	    send(I, value_set, VS)
	;   true
	).

reinitialise(I) :->
	send(I, clear).

typed(I, Id:event_id) :->
	send(I, send_super, typed, Id),
	send(I?device, activate_buttons).

selected_completion(I, Text:char_array, Apply:[bool]) :->
	send_super(I, selected_completion, Text, Apply),
	send(I?device, activate_buttons).


selection(I, Val:'name|int') :<-
	"Get selection and convert to int if possible"::
	get_super(I, selection, Val0),
	to_int(Val0, Val).
	    
to_int(Val, Int) :-
	get(@pce, convert, Val, int, Int), !.
to_int(Val, Val).


active(I, Val:bool) :->
	send(I, send_super, active, Val),
	(   Val == @off
	->  send(I, clear)
	;   send(I?window, keyboard_focus, I)
	).

status(I, Stat:{empty,filled}) :<-
	get(I, selection, Val),
	(   get(I, value_set, VS),
	    send(VS, instance_of, chain)
	->  (   send(VS, member, Val)
	    ->  Stat = filled
	    ;   Stat = empty
	    )
	;   (   Val == ''
	    ->	Stat = empty
	    ;	Stat = filled
	    )
	).

:- pce_end_class.
