:- module(map_loader, [load_map/2]).

:- use_module(library(lists)).     % Para operações com listas

file_path(Difficulty, Path) :-    
    atomic_list_concat(['../data/maps/', Difficulty, '.pl'], Path).  % Monta o caminho do arquivo com base na dificuldade

load_map(Difficulty, MapList) :-
    file_path(Difficulty, Path),
    use_module(Path),
    (
        Difficulty = facil -> Module = facil ;
        Difficulty = medio -> Module = medio ;
        Difficulty = dificil -> Module = dificil
    ),
    Module:maps(MapList).

