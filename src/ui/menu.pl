/**
@file menu.pl: Responsável pela interação visual com o usuário, exibindo os menus no terminal.
*/


/**
Declara arquivo como módulo e exporta os predicados para os outros módulos.
*/
:- module(menu, [
    exibir_menu_principal/1,
    exibir_menu_dificuldade/1,
    exibir_menu_nivel/2,
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
exibir_menu_principal(Escolha) :-
    console:limpar_tela,
    nl,
    write('\e[33m===================='), nl,
    write('\e[33m      SOKOBAN       '), nl,
    write('\e[33m===================='), nl,
    write('\e[33m1. Jogar'), nl,
    write('\e[33m2. Escolher Nível'), nl,
    write('\e[33m3. Sair'), nl,
    write('\e[33m====================\e[0m'), nl,
    write('\e[34mEscolha uma opção: \e[0m'),
    ler_numero(Escolha).


/**
Predicado que exibe o menu de seleção da dificuldade do jogo.
@param Escolha: escolha do usuário. 
*/
exibir_menu_dificuldade(Escolha) :-
    console:limpar_tela,
    nl,
    write('\e[33m======================='), nl,
    write('\e[33m ESCOLHA A DIFICULDADE '), nl,
    write('\e[33m=======================\e[0m'), nl,
    write('\e[32m1. Fácil \e[0m'), nl,
    write('\e[36m2. Médio \e[0m'), nl,
    write('\e[31m3. Difícil \e[0m'), nl,
    write('\e[33m4. Voltar'), nl,
    write('\e[33m======================\e[0m'), nl,
    write('\e[34mEscolha uma opção: \e[0m'),
    ler_numero(Escolha).


/**
Predicado que exibe o menu de seleção de um nível específico do jogo.
@param Escolha: escolha do usuário. 
@param TotalDeNiveis: Informação para o usuário saber quanto níveis tem disponíveis.
*/
exibir_menu_nivel(TotalDeNiveis, Escolha) :-
    console:limpar_tela,
    nl,
    write('\e[33m======================='), nl,
    write('\e[33m  ESCOLHA O NÍVEL  '), nl,
    write('\e[33m======================='), nl,
    write('\e[33mNíveis disponíveis: 1 a '), write(TotalDeNiveis), write('\e[0m'),nl,
    write('\e[34mDigite o número do nível: \e[0m'),
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
        write('\e[31mEntrada inválida. Por favor, digite um número: \e[0m'),
        ler_numero(Numero)
    ).