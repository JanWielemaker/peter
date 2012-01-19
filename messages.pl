/*  $Id: messages.pl,v 1.4 1999-12-08 16:04:06 jan Exp $

    Part of Bubbles
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(peter_messages, []).
:- use_module(optica(language)).

:- multifile
	user:term_expansion/2.
:- dynamic
	user:term_expansion/2.

term_expansion(term(Id, Term),	      language:term(Id,	 Term)).
term_expansion((term(Id, Term) :- B), (language:term(Id, Term) :- B)).
term_expansion(error(Id, Kind, Term), language:error(Id, Kind, Term)).


		 /*******************************
		 *	  PETER MESSAGES	*
		 *******************************/

term(file_login(Version),
     [ english(['FILE ', Version, ' login']),
       dutch(['FILE ', Version, ' login'])
     ]).

term(take_back,
     [ dutch('Terugnemen')
     ]).
term(result,
     [ dutch('Resultaat')
     ]).
term(prediction,
     [ dutch('Verwachte uitkomst')
     ]).
term(no_experiments_selected,
     [ dutch('U moet minstens twee experimenten selecteren')
     ]).
term(note_by(Name),
     [ dutch(Text)
     ]) :-
	sformat(Text, 'Aantekening by experiment ~w', [Name]).
term(quit_peter,
     [ dutch('Applicatie afsluiten'),
       english('Quit application')
     ]).
term(disable_experiments,
     [ dutch('Beeindig experimenten')
     ]).
term(finished,
     [ dutch('Klaar')
     ]).
term(replay,
     [ dutch('Afspelen')
     ]).
term(cancel_finish,
     [ dutch('Terug'),
       english('Back')
     ]).
term(password,
     [ dutch('Wachtwoord')
     ]).
term(invalid_password,
     [ dutch('Wachtwoord is niet goed')
     ]).
