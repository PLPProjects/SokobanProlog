/**
@file controller.pl: Responsável por controlar a execução do jogo.
*/


/** Declara o arquivo como um módulo e exporta o predicado iniciar_aplicacao para outros modulos.*/
:- module(controller, [iniciar_aplicacao/0]).


/** Importa os modulos de outros arquivos. */
:- use_module('ui/menu').  
:- use_module('core/map_loader'). 
:- use_module('core/game_state').
:- use_module(library(lists)).


/** 
Predicado que inicia o fluxo principal do programa.
*/
iniciar_aplicacao :-                                  
    loop_principal. 


/** 
Predicado responsável pelo loop principal do programa, executa de forma recursiva até que o usuário deseje encerrar o jogo.
*/
loop_principal :-                                            
    menu:exibir_menu_principal(Escolha),                    
    processar_escolha(Escolha),                             
    (Escolha == 3 -> ! ; loop_principal).


/** 
Predicado que processa a escolha do usuario no menu principal.
@param Escolha: A escolha do usuário. 
*/
processar_escolha(1) :-                                    
    iniciar_novo_jogo(facil).     
processar_escolha(2) :- 
    selecionar_dificuldade.
processar_escolha(3) :- 
    write('Até logo!'), nl,
    halt.
processar_escolha(_) :-
    write('\e[31mOpção inválida. \e[0m'), nl.


/** 
Predicado que carrega todos os mapas para a dificuldade especificada.
@param Dificuldade: Dificuldade especificada.
*/
iniciar_novo_jogo(Dificuldade) :-                            
    map_loader:carregar_mapa(Dificuldade, ListaMapas),
    jogar_niveis(ListaMapas).


/**
Predicado responsável por jogar os níveis da dificuldade em sequência.
*/
jogar_niveis([]).
jogar_niveis([PrimeiroMapa|RestoMapas]) :-
    % Verifica o status retornado pelo jogo
    game_state:iniciar(PrimeiroMapa, Status),
    ( Status == vitoria -> % Se venceu, continua para o próximo
        write('\e[32mNível concluído! Pressione qualquer tecla para continuar... \e[0m'), nl,
        get_single_char(_),
        jogar_niveis(RestoMapas)
    ; % Se voltou ao menu, para a sequência
        true 
    ).


/**
Predicado responsável por exibir o menu de dificuldade para que o usuário possa escolher.
*/
selecionar_dificuldade :-
    menu:exibir_menu_dificuldade(Escolha),
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
    map_loader:carregar_mapa(Dificuldade, ListaMapas),
    length(ListaMapas, TotalDeNiveis),
    menu:exibir_menu_nivel(TotalDeNiveis, NivelEscolhido),
    ( NivelEscolhido > 0, NivelEscolhido =< TotalDeNiveis ->
        nth1(NivelEscolhido, ListaMapas, MapaEscolhido),
        game_state:iniciar(MapaEscolhido, _)
    ; 
        write('\e[31mNível inválido. Tente novamente.\e[0m'), nl,
        selecionar_nivel(Dificuldade)
    ).