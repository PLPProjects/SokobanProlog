:- module(controller, [start_application/0]).

:- use_module('ui/menu').  
:- use_module('core/map_loader'). 
:- use_module('core/game_state').
:- use_module(library(lists)).

%% ==========================
%% PONTO DE ENTRADA
%% ==========================
start_application :-                                  
    main_loop. 

%% ==========================
%% LOOP PRINCIPAL
%% ==========================
main_loop :-                                            
    menu:display_main_menu(Choice),                    
    process_choice(Choice),                             
    (Choice == 3 -> ! ; main_loop).

process_choice(1) :-                                    
    start_new_game(facil).     
process_choice(2) :- 
    selecionar_dificuldade.
process_choice(3) :- 
    write('Até logo!'), nl,
    halt.
process_choice(_) :-
    write('Opção inválida.'), nl.


%% ==========================
%% INÍCIO DE NOVO JOGO (SEQUENCIAL)
%% ==========================
start_new_game(Difficulty) :-                           
    map_loader:load_map(Difficulty, MapList),
    jogar_niveis(MapList).

jogar_niveis([]).
jogar_niveis([PrimeiroMapa|RestoDosMapas]) :-
    % MODIFICADO: Verifica o status retornado pelo jogo
    game_state:start(PrimeiroMapa, Status),
    ( Status == vitoria -> % Se venceu, continua para o próximo
        write('Nível concluído! Pressione qualquer tecla para continuar...'), nl,
        get_single_char(_),
        jogar_niveis(RestoDosMapas)
    ; % Se voltou ao menu, para a sequência
        true 
    ).

%% ==========================
%% MENU DE SELEÇÃO DE NÍVEL
%% ==========================
selecionar_dificuldade :-
    menu:display_difficulty_menu(Escolha),
    processar_escolha_dificuldade(Escolha).

processar_escolha_dificuldade(1) :- selecionar_nivel(facil).
processar_escolha_dificuldade(2) :- selecionar_nivel(medio).
processar_escolha_dificuldade(3) :- selecionar_nivel(dificil).
processar_escolha_dificuldade(4).
processar_escolha_dificuldade(_) :- 
    write('Opção inválida.'), nl,
    selecionar_dificuldade.

selecionar_nivel(Dificuldade) :-
    map_loader:load_map(Dificuldade, ListaDeMapas),
    length(ListaDeMapas, TotalDeNiveis),
    menu:display_level_menu(TotalDeNiveis, NivelEscolhido),
    ( NivelEscolhido > 0, NivelEscolhido =< TotalDeNiveis ->
        nth1(NivelEscolhido, ListaDeMapas, MapaEscolhido),
        % A variável de status '_' é ignorada aqui porque, de qualquer forma,
        % o programa voltará ao menu principal ao final da execução.
        game_state:start(MapaEscolhido, _)
    ; NivelEscolhido == 0 ->
        selecionar_dificuldade
    ;
        write('Nível inválido. Tente novamente.'), nl,
        selecionar_nivel(Dificuldade)
    ).