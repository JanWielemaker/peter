/*  $Id: predict.pl,v 1.1.1.1 1999/11/18 16:24:43 jan Exp $

    Part of Peter
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(predict, []).
:- use_module(optica(language)).

:- pce_begin_class(predictor, dialog).

initialise(D, E:experiment) :->
	"Ask user to predict experiment"::
	message(predict_result, Label),
	send(D, send_super, initialise, Label),
	send(D, append, new(E2, experiment(@on))),
	send(E2, copy, E),
	send(D, append, button(ok, message(D, return, E2?answer))).

:- pce_end_class.

