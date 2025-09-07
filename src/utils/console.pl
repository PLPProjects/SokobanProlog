/**
@file console.pl: Módulo de utilitários para interações com o terminal.
*/


/**
Declara o arquivo como módulo e exporta o predicado limpar_tela para os outros módulos.
*/
:- module(console, [
    limpar_tela/0
]).

/**
Predicado que limpa a tela (utilizando um código de escape ANSI)
*/
limpar_tela :-
    write('\e[2J').