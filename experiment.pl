/*  $Id: experiment.pl,v 1.3 1999-12-01 10:09:05 jan Exp $

    Part of Peter
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(show_experiment,
	  [ compute_result/2
	  ]).
:- use_module(library(pce)).
:- use_module(annotate).
:- use_module(optica(configdb)).
:- use_module(optica(language)).
:- use_module(optica(log)).

:- pce_global(@cell_elevation, new(elevation(@nil, -1, white))).
:- pce_global(@selected_experiment_elevation,
	      new(elevation(@nil, -2, orange))).
:- pce_global(@experiment_elevation,
	      new(elevation(@nil, 2))).


resource(smallleft,	image,	image('smallleft.gif')).
resource(smallright,	image,	image('smallright.gif')).
resource(up,		image,  image('up.gif')).
resource(left,		image,  image('left.gif')).
resource(down,		image,  image('down.gif')).
resource(right,		image,  image('right.gif')).
resource(empty,		image,  image('emptybook.gif')).
resource(note,		image,  image('notebook.gif')).
resource(annotation,	image,  image('openbook.gif')).

:- pce_begin_class(experiment, figure).

variable(notes,		chain,	get, "Notes for this experiment").

:- pce_global(@experiment_format, new(format(vertical, 1, @on))).

initialise(E, Editable:[bool], Label:[name]) :->
	send(E, send_super, initialise),
	send(E, slot, notes, new(chain)),
	findall(N, config(variable(N, _)), Ns),
	length(Ns, NVars),
	send(E, border, 2),
	send(E, elevation, @experiment_elevation),
	(   config(experiment_layout(List))
	->  forall(between(1, NVars, N),
		   (   nth1(N, List, (Col,Row)),
		       new(EC, experiment_cell(N, editable := @off)),
		       get(EC, size, size(EW, EH)),
		       X is Col*EW,
		       Y is Row*EH,
		       send(E, display, EC, point(X, Y))
		   )),
	    new(LT, text(E?name?label_name, font := bold)),
	    send(LT, name, label_text),
	    get(text_item(x), height, LH),
	    MLH2 is round(-LH/2),
	    send(LT, center_y, MLH2),
	    send(E, display, LT),
	    send(E, display, new(answer_cell2), point(0, -LH)),
	    new(Controllers, chain),
	    (	config(annotate(false))
	    ->	true
	    ;	send(Controllers, append, annotate)
	    ),
	    send(E, display, new(EC, experiment_controller(Controllers))),
	    send(EC, x, E?width-EC?width),
	    send(EC, center_y, MLH2),
	    (   Editable \== @off,
		config(rearrange_experiments(true))
	    ->	send(E, display, new(EU, experiment_controller(chain(left)))),
		send(E, display, new(ED, experiment_controller(chain(right)))),
		send(ED, x, E?width-ED?width),
		send(ED, center_y, (E?height-LH)/2),
		send(EU, center_y, (E?height-LH)/2)
	    ;   true
	    )
	;   send(E, format, @experiment_format),
	    length(Ns, NVars),
	    (   Editable \== @off,
		config(rearrange_variables(EditableCell))
	    ->  true
	    ;   EditableCell = Editable
	    ),
	    forall(between(1, NVars, N),
		   send(E, display,
			experiment_cell(N, editable := EditableCell))),
	    send(E, display, new(answer_cell(Editable))),
	    (   Editable \== @off,
		config(rearrange_experiments(true))
	    ->  send(E, display, new(experiment_controller))
	    ;   true
	    )
	),
	send(E, update_buttons),
	(   Label \== @default
	->  send(E, name, Label)
	;   true
	).

name(E, Name:name) :->
	"Give the experiment a name"::
	send(E, send_super, name, Name),
	(   get(E, member, label_text, Text)
	->  true
	;   send(E, display, new(Text, text(font := bold))),
	    send(Text, hide),		% display as first
	    send(Text, name, label_text)
	),
	send(Text, string, Name?label_name).

id(E, Id:'name|int') :<-
	get(E, name, Name),
	name(Name, Chars),
	name(Id, Chars).

normalise(E) :->
	"Ensure I'm visible"::
	(   get(E, window, Window)
	->  send(Window, normalise, E)
	;   true
	).

selected_variable(E, Name:name, Value:any) :->
	"User selected a variable"::
	get(E?graphicals, find,
	    and(message(@arg1, instance_of, experiment_cell),
		@arg1?value == @nil),
	    Cell),
	send(Cell, fill_in, Name, Value),
	send(E, normalise).

take_back(E, VarName:name) :<-
	get_chain(E, graphicals, Grs),
	append(_, [L, N|_], Grs),
	(   \+ send(N, instance_of, experiment_cell)
	;   get(N, value, @nil)
	), !,
	get(L, name, VarName),
	send(L, clear),
	send(E, normalise).

answer(E, Answer:any) :->
	get(E, member, answer, AnswerCell),
	send(AnswerCell, answer, Answer),
	send(E, normalise).
answer(E, Answer:any) :<-
	get(E, member, answer, AnswerCell),
	get(AnswerCell, answer, Answer).

prediction(E, Prediction:any) :->
	get(E, member, answer, AnswerCell),
	send(AnswerCell, prediction, Prediction),
	send(E, normalise).

compute_result(E) :->
	"Determine the result and fill it in"::
	get_chain(E, graphicals, Grs),
	values_from_graphicals(Grs, Bindings),
	(   compute_result(Bindings, Result)
	->  send(E, answer, Result)
	;   send(E, report, error,
		 'Failed to compute result.  Please check the formulas')
	).

status(E, Status:{empty,filling,filled}) :<-
	"Fetch status of the experiment"::
	(   get(E?graphicals, find,
		and(message(@arg1, instance_of, experiment_cell),
		    @arg1?value == @nil),
		_)
	->  (   get(E?graphicals, find,
		    and(message(@arg1, instance_of, experiment_cell),
			@arg1?value \== @nil),
		    _)
	    ->	Status = filling
	    ;	Status = empty
	    )
	;   Status = filled
	).


values_from_graphicals([], []).
values_from_graphicals([H|T], [Name=Value|R]) :-
	send(H, instance_of, experiment_cell),
	get(H, name, Name),
	get(H, value, Value),
	Value \== @nil, !,
	values_from_graphicals(T, R).
values_from_graphicals([_|T], V) :-
	values_from_graphicals(T, V).

copy(E, E2:experiment) :->
	get_chain(E,  graphicals, Grs1),
	get_chain(E2, graphicals, Grs2),
	copy(Grs1, Grs2).

copy([H1|T1], [H2|T2]) :-
	send(H2, instance_of, experiment_cell), !,
	get(H2, name, Name),
	get(H2, value, Value),
	send(H1, fill_in, Name, Value),
	copy(T1, T2).
copy([_|T1], [_|T2]) :- !,
	copy(T1, T2).
copy(_, _).


:- pce_group(stacking).

up(E) :->
	"Move one up"::
	get(E, window, EW),
	send(EW, move_experiment, E, -1),
	get(E, id, Id),
	log(up(Id)).

down(E) :->
	"Move one down"::
	get(E, window, EW),
	send(EW, move_experiment, E, 1),
	get(E, id, Id),
	log(down(Id)).

:- pce_group(selection).

selected(E, Sel:bool) :->
	send(E, send_super, selected, Sel),
	(   Sel == @on
	->  send(E, elevation, @selected_experiment_elevation)
	;   send(E, elevation, @experiment_elevation)
	).
selected(E, Sel:bool) :<-
	(   get(E, elevation, @selected_experiment_elevation)
	->  Sel = @on
	;   Sel = @off
	).

toggle_selected(E) :->
	send(E, send_super, toggle_selected),
	get(E, selected, Sel),
	get(E, id, Id),
	(   Sel == @on
	->  log(selected(Id))
	;   log(deselected(Id))
	).

:- pce_global(@experiment_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, toggle_selected)))).

event(E, Ev:event) :->
	(   send(E, send_super, event, Ev)
	;   send(@experiment_recogniser, event, Ev)
	).

:- pce_group(annotate).

annotate(E) :->
	get(E, notes, Notes),
	get(E, frame, Frame),
	get(E, id, Id),
	message(note_by(Id), Label),
	log(begin(notebook(Id))),
	new(NB, notebook(Label, Notes)),
	send(NB, transient_for, Frame),
	send(NB, modal, transient),
	(   get(E, member, controller, Controler)
	->  send(Controler, show_open_book)
	;   true
	),
	ignore(get(NB, confirm_centered, Frame?area?center, _)),
	log(end),
	send(E, update_buttons).

update_buttons(E) :->
	(   get(E, member, controller, Controler)
	->  send(Controler, update_buttons)
	;   true
	).

:- pce_end_class.

:- pce_begin_class(experiment_cell, figure).

variable(value,		any*,	get, "Value filled-in").

initialise(C, Name:name=name, Value:value=[any], Editable:editable=[bool]) :->
	send(C, send_super, initialise),
	send(C, elevation, @cell_elevation),
	(   Value == @default
	->  send(C, clear)
	;   send(C, fill_in, Name, Value)
	),
	send(C, name, Name),
	config(free_image(FreeImg)),
	send(C, display, bitmap(FreeImg)),
	(   Editable \== @off
	->  send(C, attach_lr_buttons)
	;   true
	).

attach_lr_buttons(C) :->
	get(C, size, size(W, H)),
	new(L, button(left)),
	send(L, label, image(resource(smallleft))),
	send(L, x, 0),
	send(L, center_y, H/2),
	send(C, display, L),
	new(R, button(right)),
	send(R, label, image(resource(smallright))),
	send(R, x, W-R?width),
	send(R, center_y, H/2),
	send(C, display, R).

fill_in(C, Var:name, Value:any) :->
	"Fill slot with value"::
	send(C, name, Var),
	send(C, slot, value, Value),
	config(variable(Var, Values)),
	memberchk(Value=Image, Values),
	(   get(C, member, bitmap, BM)
	->  send(BM, image, Image)
	;   send(C, display, bitmap(Image))
	).

clear(C) :->
	"Reset the cell"::
	send(C, name, ''),
	send(C, slot, value, @nil),
	config(free_image(FreeImg)),
	(   get(C, member, bitmap, BM)
	->  send(BM, image, FreeImg)
	;   send(C, display, bitmap(FreeImg))
	).

left(E) :->
	"Move one left"::
	get(E, device, Dev),
	get(Dev, graphicals, Grs),
	get(Grs, index, E, Idx),
	(   Idx > 1,
	    get(Grs, nth1, Idx-1, Left),
	    get(E, class, Class),
	    get(Left, class, Class)
	->  send(E, swap, Left),
	    send(E, warp_to_button, left)
	;   send(E, flash),
	    fail
	).

right(E) :->
	"Move one right"::
	get(E, device, Dev),
	get(Dev, graphicals, Grs),
	get(Grs, index, E, Idx),
	(   get(Grs, nth1, Idx+1, Right),
	    get(E, class, Class),
	    get(Right, class, Class)	% force same class
	->  send(E, swap, Right),
	    send(E, warp_to_button, right)
	;   send(E, flash),
	    fail
	).

warp_to_button(E, Name:name) :->
	"Warp pointer to button"::
	send(E?window, compute),
	get(E, member, Name, Button),
	get(Button, size, size(W, H)),
	send(Button, pointer, point(W/2, H/2)).

:- pce_end_class.


:- pce_begin_class(answer_cell, figure).

initialise(C, Editable:[bool]) :->
	send(C, send_super, initialise),
	send(C, elevation, @cell_elevation),
	send(C, name, answer),
	config(free_image(FreeImg)),
	send(C, display, new(BM, bitmap(FreeImg))),
	(   Editable == @on
	->  send(C, display, new(T, int_item(answer, @default, @nil, 0, 100))),
	    send(T, show_label, @off)
	;   send(C, display, new(T, text('', center, bold)))
	),
	send(T, center, BM?center).

answer(C, Answer:any) :->
	"fill the answer"::
	get(C, member, text, T),
	send(T, string, Answer).
answer(C, Answer:any) :<-
	"Get the answer"::
	get(C, member, text, T),
	get(T, string, Answer).

prediction(C, Prediction:any) :->
	"Show prediction"::
	get(C, member, bitmap, BM),
	get(BM, right_side, R),
	get(BM, bottom_side, B),
	new(T, text(Prediction, right, normal)),
	send(T, x, R-T?width),
	send(T, y, B-T?height),
	send(T, name, prediction),
	send(C, display, T).

:- pce_end_class.

:- pce_begin_class(answer_cell2, device,
		   "Answer space to be placed in the title").

:- pce_global(@answer_cell2_format, new(format(vertical, 1, @on))).


initialise(C) :->
	send(C, send_super, initialise),
	send(C, format, @answer_cell2_format),
	send(C, name, answer).

value(C, Name:name, Answer:any) :->
	"fill the answer"::
	(   get(C, member, Name, VI),
	    send(VI, selection, Answer)
	;   send(C, display, new(VI, text_item(Name, Answer))),
	    send(VI, label_font, font(helvetica, bold, 10)),
	    (	config(label(Name, Label))
	    ->  send(VI, label, Label)
	    ;	true
	    ),
	    send(VI, editable, @off)
	),
	get(VI, print_name_of_value, Answer, Text),
	get(VI?value_font, width, Text, W),
	get(VI?value_text, border, Border),
	send(VI, value_width, W+2*Border),
	get(VI?device?device, width, Width),
	send(VI?device, center_x, Width/2 - 5).
value(C, Name:name, Answer:any) :<-
	"Get the answer"::
	get(C, member, Name, VI),
	get(VI, selection, Answer).

prediction(C, Prediction:any) :->
	send(C, value, prediction, Prediction).
prediction(C, Prediction:any) :<-
	get(C, value, prediction, Prediction).

answer(C, Answer:any) :->
	send(C, value, answer, Answer).
answer(C, Answer:any) :<-
	get(C, value, answer, Answer).

:- pce_end_class.


:- pce_begin_class(experiment_controller, device,
		   "Annotate and move an experiment").

initialise(EC, Buttons:[chain]) :->
	send(EC, send_super, initialise),
	send(EC, name, controller),
	ifbutton(Buttons, up,
		 (send(EC, append_dialog_item,
		       new(Up, button(up))),
		  send(Up, label, image(resource(up))))),
	ifbutton(Buttons, left,
		 (send(EC, append_dialog_item,
		       new(Left, button(up))),
		  send(Left, label, image(resource(left))))),
	ifbutton(Buttons, annotate,
		 (send(EC, append_dialog_item,
		       new(Annotate, button(annotate)), next_row),
		  send(Annotate, label, image(resource(empty))))),
	ifbutton(Buttons, down,
		 (send(EC, append_dialog_item,
		       new(Down, button(down)), next_row),
		  send(Down, label, image(resource(down))))),
	ifbutton(Buttons, right,
		 (send(EC, append_dialog_item,
		       new(Right, button(down)), next_row),
		  send(Right, label, image(resource(right))))),
	send(EC, layout_dialog, size(0,0), @default, size(0,0)).

ifbutton(@default, Button, G) :- !,
	(   default_button(Button)
	->  G
	;   true
	).
ifbutton(Buttons, Button, G) :-
	send(Buttons, member, Button), !,
	G.
ifbutton(_, _, _).

default_button(up).
default_button(down).
default_button(left) :- config(window_size(_, H)), H > 1.
default_button(right) :- config(window_size(_, H)), H > 1.
default_button(annotate) :- config(annotate(true)).

up(EC) :->
	send(EC?device, up),
	send(EC, warp_to_button, up).

down(EC) :->
	send(EC?device, down),
	send(EC, warp_to_button, down).

warp_to_button(EC, Name:name) :->
	"Warp pointer to button"::
	send(EC?window, normalise, EC?device),
	send(EC?window, compute),
	get(EC, member, Name, Button),
	get(Button, size, size(W, H)),
	send(Button, pointer, point(W/2, H/2)).
	
update_buttons(EC) :->
	(   get(EC, member, annotate, Annotate)
	->  (   get(EC?device, notes, Notes),
		\+ send(Notes, empty)
	    ->  send(Annotate, label, image(resource(note)))
	    ;   send(Annotate, label, image(resource(empty)))
	    )
	;   true
	).

show_open_book(EC) :->
	"Show book in open state"::
	get(EC, member, annotate, Annotate),
	send(Annotate, label, image(resource(annotation))).

annotate(EC) :->
	"Annotate the experiment"::
	send(EC?device, annotate).

:- pce_end_class.


:- pce_begin_class(experiment_window, window).

variable(experiments, chain,	  get, "Set of experiments").
variable(current,    experiment*, get, "Current experiment").
variable(last_no,    int := 0,	  get, "Latest experiment number").
variable(start,	     int := 1,	  get, "First experiment shown").
variable(rows,	     int := 4,    get, "Number of rows to use").
variable(cols,	     int := 1,    get, "Number of columns to use").

scroll_unit(N) :-
	config(scroll_unit(N)), !.
scroll_unit(1).

initialise(EW) :->
	send(EW, send_super, initialise),
	send(EW, selection_feedback, @nil),
	send(EW, recogniser, click_gesture(left, '', single,
					   message(EW, clear_selection))),
	send(EW, background, colour(cornsilk)),
	send(EW, slot, experiments, new(chain)),
	send(EW, scrollbars, vertical),
	get(EW, vertical_scrollbar, SB),
	send(SB, auto_hide, @off),
	send(EW, compute),
	get(experiment(@default, 10), area, area(_,_,W,H)),
	(   config(window_size(Rows, Cols))
	->  send(EW, slot, rows, Rows),
	    send(EW, slot, cols, Cols)
	;   get(EW, slot, rows, Rows),
	    get(EW, slot, cols, Cols)
	),
	send(EW, size, size(10+(W+10)*Cols, 10+(H+10)*Rows)).
	
move_experiment(EW, E:experiment, Places:int) :->
	"> 0: to the end, < 0: to the start"::
	get(EW, experiments, ES),
	get(ES, index, E, Idx),
	get(ES, size, N),
	NIdx is max(1, min(N, Idx+Places)),
	get(ES, nth1, NIdx, E2),
	(   Places > 0
	->  send(ES, move_after, E, E2)
	;   send(ES, move_before, E, E2)
	),
	send(EW, normalise, E).


reposition(EW) :->
	"Position the experiments"::
	get(EW, experiments, Chain),
	get(EW, cols, Cols),
	get(EW, rows, Rows),
	get(EW, start, Start),
	send(EW, clear),
	(   between(1, Rows, R),
	    between(1, Cols, C),
	    N is (R-1)*Cols+(C-1)+Start,
	    (	get(Chain, nth1, N, E)
	    ->  get(E, size, size(W, H)),
		X is 10+(C-1)*(W+10),
		Y is 10+(R-1)*(H+10),
		send(E, set, X, Y),
		send(EW, display, E),
		fail
	    ;	true
	    )
	;   true
	), !,
	send(EW, update_scroll_bar).
	    
start(EW, Start0:int) :->
	get(EW?experiments, size, N),
	get(EW, view, V),
	Start1 is max(1, min(N-V+1, Start0)), % normalise
	scroll_unit(SU),
	Start is ((Start1-1+SU//2)//SU)*SU + 1,
	(   get(EW, start, Start)
	->  true
	;   send(EW, slot, start, Start),
	    send(EW, reposition)
	),
	send(EW, update_scroll_bar).

normalise(EW, E:experiment) :->
	"Ensure E is visible"::
	get(EW, experiments, ES),
	get(ES, index, E, Index),
	get(EW, start, Start),
	(   Index < Start
	->  send(EW, start, Index)
	;   get(EW, cols, Cols),
	    get(EW, rows, Rows),
	    Index >= Start + Cols*Rows
	->  NewStart is 1+Index-Cols*Rows,
	    send(EW, start, NewStart)
	;   send(EW, reposition)	% ???
	).

update_scroll_bar(EW) :->
	(   get(EW, vertical_scrollbar, SB),
	    SB \== @nil
	->  send(SB, request_compute)
	;   true
	).

bubble_scroll_bar(EW, SB:[scroll_bar]) :->
	"Update the vertical scrollbar"::
	(   get(EW, vertical_scrollbar, SB)
	->  get(EW, start, Start),
	    get(EW, cols, Cols),
	    get(EW, rows, Rows),
	    View is Rows * Cols,
	    get(EW?experiments, size, Length),
	    send(SB, bubble, Length, Start-1, View)
	;   true
	).
	    
changed_union(_EW, _Args:int ...) :->
	"Avoid window from updating scrollbar"::
	true.

view(E, View:int) :<-
	"Max number of viewable cells"::
	get(E, rows, Rows),
	get(E, cols, Cols),
	View is Rows * Cols.

scroll_vertical(EW, D:{forwards,backwards,goto}, U:{page,file,line}, A:int) :->
	"Handle a scroll request"::
	get(EW, start, Start0),
	scroll_vertical(EW, D, U, A),
	get(EW, start, Start),
	(   Start0 \== Start
	->  get(EW, view, View),
	    End is Start+View-1,
	    get(EW, experiments, ES),
	    findall(Id, id_in_range(ES, Start, End, Id), Ids),
	    log(scroll(Ids))
	;   true
	).

id_in_range(Chain, Start, End, Id) :-
	between(Start, End, N),
	get(Chain, nth1, N, E),
	get(E, id, Id).

scroll_vertical(EW, forwards, line, _) :- !,
	scroll_unit(N),
	send(EW, start, EW?start+N).
scroll_vertical(EW, backwards, line, _) :- !,
	scroll_unit(N),
	send(EW, start, EW?start-N).
scroll_vertical(EW, backwards, page, _) :- !,
	get(EW, view, View),
	send(EW, start, EW?start-View).
scroll_vertical(EW, forwards, page, _) :- !,
	get(EW, view, View),
	send(EW, start, EW?start+View).
scroll_vertical(EW, goto, file, Amount) :-
	get(EW?experiments, size, N),
	Start is round((N * Amount)/1000),
	send(EW, start, Start).

append(EW, E:experiment) :->
	"Append an experiment"::
	send(EW?experiments, append, E),
	send(EW, normalise, E).

new_experiment(EW) :->
	"Prepare for a new experiment"::
	new(E, experiment),
	get(EW, last_no, LastNo),
	No is LastNo+1,
	send(EW, slot, last_no, No),
	send(E, name, No),
	send(EW, append, E),
	send(EW, slot, current, E).

reinitialise(EW) :->
	"Reset to initial state"::
	send_super(EW, clear),
	send(EW?experiments, clear),
	send(EW, slot, current, @nil),
	send(EW, slot, last_no, 0).

:- pce_group(selection).

show_selection(EW) :->
	(   get(EW, selection, Experiments),
	    get(Experiments, size, NSelected),
	    NSelected > 1
	->  get(EW, open_selection, Experiments, _Frame),
	    selection_list(Experiments, List),
	    log(begin(show_selection(List)))
	;   message(no_experiments_selected, Msg),
	    send(EW?display, inform, Msg)
	).

end_show_selection(EW) :->
	send(EW, reposition),
	log(end).

open_selection(EW, Experiments:chain, F:frame) :<-
	"Open experiments in window"::
	new(F, frame),
	send(F, append, new(EW2, experiment_window)),
	send(EW2?vertical_scrollbar, auto_hide, @on),
	send(new(D, dialog), below, EW2),
	send(D, resize_message, message(D, layout, @arg2)),
	send(D, append, button(quit,
			       and(message(F, free),
				   message(EW, end_show_selection)))),
	send(Experiments, for_all,
	     message(EW2, append, @arg1)),
	send(EW2, start, 1),
	send(F, transient_for, EW?frame),
	send(F, modal, transient),
	send(F, open_centered, EW?frame?area?center).

selection_list(Chain, List) :-
	get(Chain, map, @arg1?id, Names),
	chain_list(Names, List).

selection(EW, Selection:chain) :<-
	get(EW, experiments, ES),
	get(ES, find_all, @arg1?selected == @on, Selection).

selection(EW, Selection:'experiment|chain*') :->
	get(EW, experiments, ES),
	(   Selection == @nil
	->  send(ES, for_all, message(@arg1, selected, @off))
	;   send(Selection, instance_of, chain)
	->  send(ES, for_all,
		 if(message(Selection, member, @arg1),
		    message(@arg1, selected, @on),
		    message(@arg1, selected, @off)))
	;   send(ES, for_all,
		 if(Selection == @arg1,
		    message(@arg1, selected, @on),
		    message(@arg1, selected, @off)))
	).

clear_selection(EW) :->
	(   get(EW, selection, Sel),
	    \+ send(Sel, empty)
	->  send(EW, send_super, selection, @nil),
	    log(clear_selection)
	;   true
	).

:- pce_end_class.

		 /*******************************
		 *	       COMPUTE		*
		 *******************************/

compute_result(Bindings, Result) :-
	compute(Bindings, $result, Result).

compute(Bindings, $Var, Value) :-
	memberchk(Var = Value, Bindings), !.
compute(Bindings, $Var, Value) :-
	Binding =.. [Var, Value],
	memberchk(Binding, Bindings), !.
compute(Bindings, $Var, Value) :- !,
	config(formula(Condition, $Var = Value0)),
	eval_condition(Condition, Bindings), !,
	eval_expression(Bindings, Value0, Value).
compute(_, Value, Value).

eval_condition(And, Bindings) :-
	functor(And, and, _), !,
	forall(arg(_, And, Arg), eval_condition(Arg, Bindings)).
eval_condition(Or, Bindings) :-
	functor(Or, or, _), !,
	arg(_, Or, Arg),
	eval_condition(Arg, Bindings), !.
eval_condition(A = OneOfSet, Bindings) :-
	functor(OneOfSet, oneof, _), !,
	compute(Bindings, A, ValA),
	arg(_, OneOfSet, Arg),
	compute(Bindings, Arg, ValArg),
	ValA = ValArg.
eval_condition(A = B, Bindings) :- !,
	compute(Bindings, A, ValA),
	compute(Bindings, B, ValB),
	ValA = ValB.
eval_condition(Goal, Bindings) :- !,
	Goal =.. [Functor|Args0],
	maplist(compute(Bindings), Args0, Args),
	TheGoal =.. [Functor|Args],
	TheGoal.

eval_expression(Bindings, $Var, Value) :- !,
	compute(Bindings, $Var, Value).
eval_expression(Bindings, Term, Value) :-
	compound(Term),
	current_arithmetic_function(Term), !,
	Term =.. [Op|Args0],
	maplist(eval_expression(Bindings), Args0, Args),
	NewTerm =.. [Op|Args],
	Value is NewTerm.
eval_expression(_, Value, Value).
