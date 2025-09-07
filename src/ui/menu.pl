/**
@file menu.pl: Responsável pela interação visual com o usuário, exibindo os menus no terminal.
*/

:- module(menu, [
    display_main_menu/1,
    display_difficulty_menu/1,
    display_level_menu/2,
    ler_numero/1
]).
:- use_module('../utils/console').

display_main_menu(Escolha) :-
    console:limpar_tela,
    nl,
    write('===================='), nl,
    write('      SOKOBAN       '), nl,
    write('===================='), nl,
    write('1. Jogar'), nl,
    write('2. Escolher Nível'), nl,
    write('3. Sair'), nl,
    write('===================='), nl,
    write('Escolha uma opção: '),
    ler_numero(Escolha).

display_difficulty_menu(Escolha) :-
    console:limpar_tela,
    nl,
    write('======================='), nl,
    write(' ESCOLHA A DIFICULDADE '), nl,
    write('======================='), nl,
    write('1. Fácil'), nl,
    write('2. Médio'), nl,
    write('3. Difícil'), nl,
    write('4. Voltar'), nl,
    write('======================'), nl,
    write('Escolha uma opção: '),
    ler_numero(Escolha).

display_level_menu(TotalDeNiveis, Escolha) :-
    console:limpar_tela,
    nl,
    write('======================='), nl,
    write('  ESCOLHA O NÍVEL  '), nl,
    write('======================='), nl,
    write('Níveis disponíveis: 1 a '), write(TotalDeNiveis), nl,
    write('Digite o número do nível: '),
    ler_numero(Escolha).

ler_numero(Numero) :-
    read_string(user_input, "\n", "\r\t ", _, String),
    ( number_string(Numero, String) ->
        true
    ; 
        write('Entrada inválida. Por favor, digite um número: '),
        ler_numero(Numero)
    ).