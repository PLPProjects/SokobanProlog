:- module(game_state, [
   start/2
]).

:- use_module('../utils/console').

%% ==========================
%% INICIALIZAÇÃO DO JOGO
%% ==========================
start(Mapa, Status) :- 
    Mapa = map(TileMap, Marks, _Difficulty),
    find_player(TileMap, PlayerPos),
    game_loop(TileMap, Mapa, Marks, PlayerPos, [], Status).

%% ==========================
%% LOOP DO JOGO 
%% ==========================
game_loop(Map, OriginalMap, Marks, PlayerPos, Historico, Status) :-
    console:limpar_tela,
    print_tile_map(Map),
    print_controls,
    ( checa_vitoria(Map, Marks) ->
        print_you_win,
        Status = vitoria,
        !
    ;
        capture_move(Move),
        process_input(Move, Map, OriginalMap, Marks, PlayerPos, Historico, Status)
    ).

%% ==========================
%% PROCESSAMENTO DA ENTRADA
%% ==========================
process_input('m', _, _, _, _, _, voltou_menu) :- !.

process_input('r', _, OriginalMap, _, _, _, Status) :- 
    start(OriginalMap, Status), !.

process_input('z', Map, OriginalMap, Marks, PlayerPos, [], Status) :- 
    write('Não é possível desfazer mais!'), nl,
    sleep(1),
    game_loop(Map, OriginalMap, Marks, PlayerPos, [], Status).

process_input('z', _, OriginalMap, Marks, _, [state(MapaAnterior, PosAnterior)|RestoHistorico], Status) :-
    game_loop(MapaAnterior, OriginalMap, Marks, PosAnterior, RestoHistorico, Status), !.

process_input(Move, Map, OriginalMap, Marks, PlayerPos, Historico, Status) :-
    move_player(Map, Marks, PlayerPos, Move, NovoMapa, NovoPlayerPos),
    ( PlayerPos \== NovoPlayerPos -> 
        
        NovoHistorico = [state(Map, PlayerPos) | Historico]
    ; 
        NovoHistorico = Historico
    ),
    game_loop(NovoMapa, OriginalMap, Marks, NovoPlayerPos, NovoHistorico, Status).


%% ==========================
%% ENCONTRA O JOGADOR
%% ==========================
find_player(Map, (Row, Col)) :-
    nth0(Row, Map, Line),
    nth0(Col, Line, jogador), !.


%% ==============================
%% VERIFICAÇÃO DE TIPO DA POSIÇÃO
%% ==============================
eh_parede(Map,Linha,Coluna) :-
    nth0(Linha, Map, Line),
    nth0(Coluna, Line, Cell),
    Cell == parede.

eh_caixa(Map,Linha,Coluna) :-
    nth0(Linha, Map, Line),
    nth0(Coluna, Line, Cell),
    Cell == caixa.

eh_marca(Marks, Linha, Coluna) :-
    member([Linha, Coluna], Marks).



%% ==========================
%% MOVIMENTO 
%% ==========================
delta(w, -1, 0).
delta(s, 1, 0).
delta(a, 0, -1).
delta(d, 0, 1).

move_player(Map, Marks, (Row, Col), Move, NewMap, NewPlayerPos) :-
    delta(Move, DY, DX),
    NewRow is Row + DY,
    NewCol is Col + DX,

    (\+ eh_parede(Map, NewRow, NewCol) ->
        (eh_caixa(Map, NewRow, NewCol) ->
            Row_depois_da_caixa is NewRow + DY,
            Col_depois_da_caixa is NewCol + DX,
            (\+eh_caixa(Map, Row_depois_da_caixa, Col_depois_da_caixa),\+eh_parede(Map, Row_depois_da_caixa, Col_depois_da_caixa) ->
                (eh_marca(Marks, Row, Col) -> 
                    update_map(Map, (Row, Col), marca, TempMap)
                ;
                    update_map(Map, (Row, Col), vazio, TempMap)
                ),
                update_map(TempMap, (NewRow, NewCol), jogador, TempMap2),
                update_map(TempMap2, (Row_depois_da_caixa, Col_depois_da_caixa), caixa, NewMap),
                NewPlayerPos = (NewRow, NewCol)
            ;
                NewMap = Map,
                NewPlayerPos = (Row, Col)
            )
        ;
        (eh_marca(Marks, Row, Col) -> 
            update_map(Map, (Row, Col), marca, TempMap)
        ;
            update_map(Map, (Row, Col), vazio, TempMap)
        ),
        update_map(TempMap, (NewRow, NewCol), jogador, NewMap),
        NewPlayerPos = (NewRow, NewCol)
        )
    ;
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

print_cell(parede)  :- write('█').
print_cell(vazio)   :- write(' ').
print_cell(jogador) :- write('@').
print_cell(caixa)   :- write('B').
print_cell(marca)   :- write('x').


print_you_win :-
    write('VITÓRIA'), nl.

print_controls :-
    nl,
    write('Controles: [W,A,S,D] Movimentam | [Z] Desfazer | [R] Resetar | [M] Voltar ao Menu'), nl.

%% ==========================
%% CAPTURA MOVIMENTO
%% ==========================
capture_move(Move) :-
    flush_output,
    get_single_char(Code),
    char_code(Char, Code),
    downcase_atom(Char, Lower),
    
    ( member(Lower, [w,a,s,d,r,m,z]) ->
        Move = Lower
    ; 
      capture_move(Move)
    ).