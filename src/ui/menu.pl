:- module(menu, [display_main_menu/1]).

display_main_menu(Choice) :-
    format("MENU~n"),
    format("1 - Iniciar Jogo~n"),
    format("2 - Escolher Dificuldade~n"),
    format("3 - Sair~n"),
    format("Opção: "),
    read(Choice).  % lê o input do usuário e atribui a Choice