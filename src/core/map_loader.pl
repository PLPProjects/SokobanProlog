/**
 @file map_loader.pl: Responsavel por carregar os arquivos de mapa do jogo.
 */


/** Declara o arquivo como um módulo e exporta o predicado load_map para os outros modulos.*/
:- module(map_loader, [carregar_mapa/2]).


/**
Importa a biblioteca lists para operações com listas
*/
:- use_module(library(lists)).     

/**
Declara que o predicado 'mapas/1' é dinamico, permitindo que ele seja limpo e redefinido a cada carregamento de mapa.
*/
:- dynamic(mapas/1).

/**
Predicado que constrói o caminho completo para o arquivo do mapa.
@param Dificuldade: A dificuldade do mapa.
@param Caminho: O caminho completo do arquivo que sera unificado.
*/
caminho_arquivo(Dificuldade, Caminho) :-    
    atomic_list_concat(['../data/maps/', Dificuldade, '.pl'], Caminho).  


/**
Predicado que carrega um mapa e retorna a lista de niveis.
@param Dificuldade: A dificuldade do mapa.
@param ListaNiveis: A lista de niveis unificada com o predicado 'maps' do arquivo. 
*/
carregar_mapa(Dificuldade, ListaNiveis) :-
    retractall(mapas(_)),
    caminho_arquivo(Dificuldade, Caminho),
    consult(Caminho),
    mapas(ListaNiveis).

