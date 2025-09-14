/**
@file main.pl: Responsável por configurar o ambiente, carregar os módulos principais e iniciar o fluxo de execução do programa de forma automática.
*/


/** Define a codificação de caracteres do Prolog para UTF-8*/
:- set_prolog_flag(encoding, utf8).


/** Carrega o arquivo controller.pl.*/
:- [controller].


/** Inicia a execução do programa chamando o predicado iniciar_aplicacao que vem do arquivo controller.pl*/
executar :-
    iniciar_aplicacao.  % Inicia o fluxo principal da aplicação