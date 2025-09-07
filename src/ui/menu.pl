/**
@file menu.pl: Responsável pela interação visual com o usuário, exibindo os menus no terminal.
*/

/**
Declara arquivo como módulo e exporta os predicados para os outros módulos.
*/
:- module(menu, [
    display_main_menu/1,
    display_difficulty_menu/1,
    display_level_menu/2,
    ler_numero/1
]).

/**
Importa o módulo console.
*/
:- use_module('../utils/console').

/**
Predicado que exibe o menu principal do jogo.
@param Escolha: escolha do usuário. 
*/
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

/**
Predicado que exibe o menu de seleção da dificuldade do jogo.
@param Escolha: escolha do usuário. 
*/
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

/**
Predicado que exibe o menu de seleção de um nível específico do jogo.
@param Escolha: escolha do usuário. 
@param TotalDeNiveis: Informação para o usuário saber quanto níveis tem disponíveis.
*/
display_level_menu(TotalDeNiveis, Escolha) :-
    console:limpar_tela,
    nl,
    write('======================='), nl,
    write('  ESCOLHA O NÍVEL  '), nl,
    write('======================='), nl,
    write('Níveis disponíveis: 1 a '), write(TotalDeNiveis), nl,
    write('Digite o número do nível: '),
    ler_numero(Escolha).

/**
Predicado que lê a entrada do usuário e válida se é um número.
@param Numero: Entrada digitada pelo usuário, se não for um número é pedido novamente.
*/
ler_numero(Numero) :-
    read_string(user_input, "\n", "\r\t ", _, String),
    ( number_string(Numero, String) ->
        true
    ; 
        write('Entrada inválida. Por favor, digite um número: '),
        ler_numero(Numero)
    ).