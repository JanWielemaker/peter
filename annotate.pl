/*  $Id: annotate.pl,v 1.1.1.1 1999/11/18 16:24:43 jan Exp $

    Part of Peter
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/


:- module(annotate, []).
:- use_module(library(pce)).
:- use_module(optica(language)).
:- use_module(optica(log)).

:- pce_begin_class(notebook, frame,
		   "Peter's notebook").

variable(notes,		chain,	get, "Associated notes").
variable(editor,	editor, get, "Editor for new note").

initialise(NB, Label:name, Notes:chain) :->
	send(NB, send_super, initialise, Label),
	send(NB, append, new(dialog)),
	send(NB, notes, Notes).

notes(NB, Notes:chain) :->
	get(NB, member, dialog, D),
	send(D, clear),
	send(NB, slot, notes, Notes),
	get(NB, label, Label),
	send(D, append, label(comment, Label)),
	send(Notes, for_all, message(NB, append_note, @arg1)),
	send(NB, append_editor),
	send(D, append, button(save, message(NB, ok))),
	send(D, append, button(cancel, message(NB, cancel))),
	send(NB, fix_text_widths).

append_note(NB, Note:string) :->
	get(NB, member, dialog, Dialog),
	get(NB, notes, Notes),
	get(Notes, index, Note, N),
	new(LB, label_box(N, @nil)),
	send(LB, append, text(Note)),
	send(Dialog, append, LB).

append_editor(NB) :->
	get(NB, member, dialog, D),
	get(NB?notes, size, Size),
	Label is Size+1,
	new(LB, label_box(Label, @nil)),
	send(LB, append, new(E, editor(@default, 50, 10))),
	send(E, font, normal),
	send(NB, slot, editor, E),
	send(D, append, LB).
	
fix_text_widths(NB) :->
	get(NB, member, dialog, D),
	get(NB, editor, E),
	get(E?area, width, EW),
	send(D?graphicals, for_all,
	     if(and(message(@arg1, instance_of, label_box),
		    ?(@arg1, member, text)),
		message(?(@arg1, member, text), margin, EW, wrap))),
	send(D, layout),
	get(D, bounding_box, BB),
	get(BB, height, H),
	(   H > 400
	->  send(D, size, size(BB?width+40, 400)),
	    send(D, scrollbars, vertical),
	    send(D, resize_message,
		 message(D, scroll_vertical, goto, file, 1000))
	;   true
	).


cancel(NB) :->
	send(NB, destroy).

ok(NB) :->
	"Save the note"::
	get(NB, editor, E),
	get(E, contents, String),
	get(String, value, Text),
	log(note(Text)),
	send(String, strip),
	(   get(String, size, 0)
	->  send(NB, destroy)
	;   send(NB?notes, append, String),
	    send(NB, destroy)
	).

:- pce_end_class.
