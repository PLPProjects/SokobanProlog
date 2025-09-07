/**
@file controller.pl: Responsável por controlar a execução do jogo.
*/

/** Declara o arquivo como um módulo e exporta o predicado start_application para outros modulos.*/
:- module(controller, [start_application/0]).

/** Importa os modulos de outros arquivos. */
:- use_module('ui/menu').  
:- use_module('core/map_loader'). 
:- use_module('core/game_state').
:- use_module(library(lists)).

/** 
Predicado que inicia o fluxo principal do programa.
*/
start_application :-                                  
    main_loop. 

/** 
Predicado responsável pelo loop principal do programa, executa de forma recursiva até que o usuário deseje encerrar o jogo.
*/
main_loop :-                                            
    menu:display_main_menu(Choice),                    
    process_choice(Choice),                             
    (Choice == 3 -> ! ; main_loop).

/** 
Predicado que processa a escolha do usuario no menu principal.
@param Escolha: A escolha do usuário. 
*/
process_choice(1) :-                                    
    start_new_game(facil).     
process_choice(2) :- 
    selecionar_dificuldade.
process_choice(3) :- 
    write('Até logo!'), nl,
    halt.
process_choice(_) :-
    write('\e[31mOpção inválida. \e[0m'), nl.

/** 
Predicado que carrega todos os mapas para a dificuldade especificada.
@param Dificuldade: Dificuldade especificada.
*/
start_new_game(Difficulty) :-                           
    map_loader:load_map(Difficulty, MapList),
    jogar_niveis(MapList).

/**
Predicado responsável por jogar os níveis da dificuldade em sequência.
*/
jogar_niveis([]).
jogar_niveis([PrimeiroMapa|RestoDosMapas]) :-
    % MODIFICADO: Verifica o status retornado pelo jogo
    game_state:start(PrimeiroMapa, Status),
    ( Status == vitoria -> % Se venceu, continua para o próximo
        write('\e[32mNível concluído! Pressione qualquer tecla para continuar... \e[0m'), nl,
        get_single_char(_),
        jogar_niveis(RestoDosMapas)
    ; % Se voltou ao menu, para a sequência
        true 
    ).

/**
Predicado responsável por exibir o menu de dificuldade para que o usuário possa escolher.
*/
selecionar_dificuldade :-
    menu:display_difficulty_menu(Escolha),
    processar_escolha_dificuldade(Escolha).

/**
Predicado que processa a escolha da dificuldade do jogo escolhida pelo usuário.
@param Escolha: A escolha do usuário.
*/
processar_escolha_dificuldade(1) :- selecionar_nivel(facil).
processar_escolha_dificuldade(2) :- selecionar_nivel(medio).
processar_escolha_dificuldade(3) :- selecionar_nivel(dificil).
processar_escolha_dificuldade(4). % Fato 
processar_escolha_dificuldade(_) :- 
    write('\e[31mOpção inválida.\e[0m'), nl,
    selecionar_dificuldade.

/**
Predicado responsável por processar a escolha do nível da dificuldade escolhida pelo usuário.
@param Dificuldade: a dificuldade escolhida pelo usuário.
*/
selecionar_nivel(Dificuldade) :-
    map_loader:load_map(Dificuldade, ListaDeMapas),
    length(ListaDeMapas, TotalDeNiveis),
    menu:display_level_menu(TotalDeNiveis, NivelEscolhido),
    ( NivelEscolhido > 0, NivelEscolhido =< TotalDeNiveis ->
        nth1(NivelEscolhido, ListaDeMapas, MapaEscolhido),
        % A variável de status '_' é ignorada aqui porque, de qualquer forma,
        % o programa voltará ao menu principal ao final da execução.
        game_state:start(MapaEscolhido, _)
    ; 
        write('\e[31mNível inválido. Tente novamente.\e[0m'), nl,
        selecionar_nivel(Dificuldade)
    ).