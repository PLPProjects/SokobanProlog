:- module(game_state, [
   start/1  % Inicia o estado do jogo com uma lista de mapas
]).

%% ==========================
%% INICIALIZAÇÃO DO JOGO
%% ==========================
start([PrimeiroMapa|_]) :- %por enquanto ta só pegando o primeiro mapa pra teste
    PrimeiroMapa = map(TileMap, Marks, _Difficulty),
    find_player(TileMap, PlayerPos),
    game_loop(TileMap, Marks, PlayerPos).

%% ==========================
%% LOOP DO JOGO 
%% ==========================
game_loop(Map, Marks, PlayerPos) :-
    print_tile_map(Map),
    ( checa_vitoria(Map, Marks) ->
        print_you_win,
        !        %interrompe o loop quando vence
    ;
        capture_move(Move),
        move_player(Map, PlayerPos, Move, NewMap, NewPlayerPos),
        game_loop(NewMap, Marks, NewPlayerPos)
    ).

%% ==========================
%% ENCONTRA O JOGADOR
%% ==========================
find_player(Map, (Row, Col)) :-
    nth0(Row, Map, Line),
    nth0(Col, Line, jogador), !.


%% ==============================
%% VERIFICAÇÃO DE TIPO DA POSIÇÃO
%% ==============================
% verifica se a posição é uma parede
eh_parede(Map,Linha,Coluna) :-
    nth0(Linha, Map, Line),
    nth0(Coluna, Line, Cell),
    Cell == parede.

% verifica se a posição é uma caixa
eh_caixa(Map,Linha,Coluna) :-
    nth0(Linha, Map, Line),
    nth0(Coluna, Line, Cell),
    Cell == caixa.

% verifica se a posição é uma marca
eh_marca(Marks, Linha, Coluna) :-
    %verifica se a coordenada recebida está na lista de coordenadas do mapa
    member([Linha, Coluna], Marks).
    

%% ==========================
%% MOVIMENTO 
%% ==========================
delta(w, -1, 0).  % cima
delta(s, 1, 0).   % baixo
delta(a, 0, -1).  % esquerda
delta(d, 0, 1).   % direita

move_player(Map, (Row, Col), Move, NewMap, NewPlayerPos) :-
    % encontra posição+1 (posição+1 É onde o personagem fica caso o movimento seja válido)
    delta(Move, DY, DX),
    NewRow is Row + DY,
    NewCol is Col + DX,

    (\+ eh_parede(Map, NewRow, NewCol) -> % se posição+1 NÃO é parede
        (eh_caixa(Map, NewRow, NewCol) -> % se posição+1 é caixa
            % encontra posição+2 (posição+2 É onde a caixa fica caso ela possa ser empurrada)
            Row_depois_da_caixa is NewRow + DY,
            Col_depois_da_caixa is NewCol + DX,
            (\+eh_caixa(Map, Row_depois_da_caixa, Col_depois_da_caixa),\+eh_parede(Map, Row_depois_da_caixa, Col_depois_da_caixa) -> %se a posição+2 não for nem parede nem caixa a caixa na posição+1 é empurrada
                update_map(Map, (Row, Col), vazio, TempMap),
                update_map(TempMap, (NewRow, NewCol), jogador, TempMap2),
                update_map(TempMap2, (Row_depois_da_caixa, Col_depois_da_caixa), caixa, NewMap),
                NewPlayerPos = (NewRow, NewCol)
            ;
                NewMap = Map,
                NewPlayerPos = (Row, Col)
            )
        ;
        % se a posição+1 não for caixa, realiza movimento normalmente
        update_map(Map, (Row, Col), vazio, TempMap),
        update_map(TempMap, (NewRow, NewCol), jogador, NewMap),
        NewPlayerPos = (NewRow, NewCol)
        )
    ;%posição+1 é parede
    NewMap = Map,
    NewPlayerPos = (Row, Col)
    ).

%% ==========================
%% CHECA VITÓRIA
%% ==========================
checa_vitoria(Map, Marks) :-
    forall(
        member([Linha, Coluna], Marks),
        eh_caixa(Map, Linha, Coluna)
    ).



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


print_you_win :-
    write('VITÓRIA'), nl.

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

