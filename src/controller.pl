:- module(controller, [start_application/0]).

:- use_module('ui/menu').  
:- use_module('core/map_loader'). 
:- use_module('core/game_state').

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
    main_loop.

process_choice(1) :-                                    
    start_new_game(facil).     
process_choice(2) :- write('tem q fazer o escolher dificuldade'), nl.
process_choice(3) :- halt.

%% ==========================
%% INÍCIO DE NOVO JOGO
%% ==========================
start_new_game(Difficulty) :-                           
    map_loader:load_map(Difficulty, Game),        
    game_state:start(Game).


