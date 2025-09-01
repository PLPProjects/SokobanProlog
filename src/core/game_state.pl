:- module(game_state, [
   start/1  % Inicia o estado do jogo com uma lista de mapas
]).

%% ==========================
%% INICIALIZAÇÃO DO JOGO
%% ==========================
start([PrimeiroMapa|_]) :- %por enquanto ta só pegando o primeiro mapa pra teste
    PrimeiroMapa = map(TileMap, _Marks, _Difficulty),
    find_player(TileMap, PlayerPos),
    game_loop(TileMap, PlayerPos).

%% ==========================
%% LOOP DO JOGO 
%% ==========================
game_loop(Map, PlayerPos) :-
    print_tile_map(Map),
    capture_move(Move),
    move_player(Map, PlayerPos, Move, NewMap, NewPlayerPos),
    game_loop(NewMap, NewPlayerPos).

%% ==========================
%% ENCONTRA O JOGADOR
%% ==========================
find_player(Map, (Row, Col)) :-
    nth0(Row, Map, Line),
    nth0(Col, Line, jogador), !.

%% ==========================
%% MOVIMENTO (n ta considerando parede nem caixa)
%% ==========================
delta(w, -1, 0).  % cima
delta(s, 1, 0).   % baixo
delta(a, 0, -1).  % esquerda
delta(d, 0, 1).   % direita

move_player(Map, (Row, Col), Move, NewMap, (NewRow, NewCol)) :-
    delta(Move, DY, DX),
    NewRow is Row + DY,
    NewCol is Col + DX,
    % limpa posição antiga
    update_map(Map, (Row, Col), vazio, TempMap),
    % coloca jogador na nova posição
    update_map(TempMap, (NewRow, NewCol), jogador, NewMap).

%% ==========================
%% ATUALIZAÇÃO DO MAPA
%% ==========================
update_map(Map, (Row, Col), Value, NewMap) :-
    nth0(Row, Map, OldLine, RestLines),
    nth0(Col, OldLine, _OldValue, RestCells),
    nth0(Col, NewLine, Value, RestCells),
    nth0(Row, NewMap, NewLine, RestLines).

%% ==========================
%% IMPRESSÃO DO MAPA
%% ==========================
print_tile_map([]).
print_tile_map([Linha|Resto]) :-
    print_line(Linha),
    print_tile_map(Resto).

print_line([]) :- nl.
print_line([Celula|Resto]) :-
    print_cell(Celula),
    print_line(Resto).

print_cell(parede)  :- write('#').
print_cell(vazio)   :- write(' ').
print_cell(jogador) :- write('@').
print_cell(caixa)   :- write('B').
print_cell(marca)   :- write('x').

%% ==========================
%% CAPTURA MOVIMENTO
%% ==========================
capture_move(Move) :-
    flush_output,
    get_single_char(Code),
    char_code(Char, Code),           % transforma código ASCII em caractere
    downcase_atom(Char, Lower),      % converte para minúscula
    ( member(Lower, [w,a,s,d]) ->
        Move = Lower
    ; 
      capture_move(Move)
    ).

