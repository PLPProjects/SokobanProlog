
:- set_prolog_flag(encoding, utf8).

:- [controller].

:- initialization(run).

run :-
    start_application.  % Inicia o fluxo principal da aplicação