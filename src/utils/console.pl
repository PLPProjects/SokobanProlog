% utils/console.pl
:- module(console, [
    limpar_tela/0
]).

limpar_tela :-
    write('\e[2J').