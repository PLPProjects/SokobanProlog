/**
 @file map_loader.pl: Responsavel por carregar os arquivos de mapa do jogo.
 */

/** Declara o arquivo como um módulo e exporta o predicado load_map para os outros modulos.*/
:- module(map_loader, [load_map/2]).

/**
Importa a biblioteca lists para operações com listas
*/
:- use_module(library(lists)).     

/**
Predicado que constrói o caminho completo para o arquivo do mapa.
@param Dificuldade: A dificuldade do mapa.
@param Caminho: O caminho completo do arquivo que sera unificado.
*/
file_path(Difficulty, Path) :-    
    atomic_list_concat(['../data/maps/', Difficulty, '.pl'], Path).  

/**
Predicado que carrega um mapa e retorna a lista de niveis.
@param Dificuldade: A dificuldade do mapa.
@param ListaNiveis: A lista de niveis unificada com o predicado 'maps' do arquivo. 
*/
load_map(Difficulty, MapList) :-
    file_path(Difficulty, Path),
    use_module(Path),
    (
        Difficulty = facil -> Module = facil ;
        Difficulty = medio -> Module = medio ;
        Difficulty = dificil -> Module = dificil
    ),
    Module:maps(MapList).