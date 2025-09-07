/**
@file main.pl: Responsável por configurar o ambiente, carregar os módulos principais e iniciar o fluxo de execução do programa de forma automática.
*/

/** Define a codificação de caracteres do Prolog para UTF-8*/
:- set_prolog_flag(encoding, utf8).

/** Carrega o arquivo controller.pl.*/
:- [controller].

/** Inicializa o programa chamando o predicado run*/
:- initialization(run).

/** Inicia a execução do programa chamando o predicado start_application que vem do arquivo controller.pl*/
run :-
    start_application.  % Inicia o fluxo principal da aplicação