/**
@file game_state.pl: Modulo principal que gerencia a lógica do jogo.
Responsável pelo loop principal, movimentos, checagem de vitória e manipulação do estado do jogo.
*/

/** 
Declara o arquivo como um módulo e exporta o predicado start para os outros modulos.
*/
:- module(game_state, [
   start/2
]).

/**
Importa o módulo console.
*/
:- use_module('../utils/console').

/**
Predicado que inicia um novo jogo.
@param Mapa: O mapa a ser jogado.
@param Status: A variavel para unificar com o status final do jogo.
*/
start(Mapa, Status) :- 
    Mapa = map(TileMap, Marks, _Difficulty),
    find_player(TileMap, PlayerPos),
    game_loop(TileMap, Mapa, Marks, PlayerPos, [], Status).

/**
Preciado que inicia o loop principal do jogo.
@param Mapa: A representacao atual do tabuleiro.
@param MapaOriginal: O mapa original.
@param Marks: As posicoes dos alvos.
@param PosJogador: A posicao atual do jogador.
@param Historico: A lista de estados anteriores para desfazer movimentos.
@param Status: A variavel que sera unificada com o status final do jogo.
*/
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

/**
Predicado que processa a entrada do usuario.
@param Move: O comando de movimento.
@param Mapa, MapaOriginal, Marks, PosJogador, Historico: Dados do estado.
@param Status: Status do jogo.
*/
/**Se a entrada for 'm' e volta para o menu. */
process_input('m', _, _, _, _, _, voltou_menu) :- !.

/**Se a entrada for 'r' o jogo é resetado. */
process_input('r', _, OriginalMap, _, _, _, Status) :- 
    start(OriginalMap, Status), !.

/**Se a entrada for 'z' o movimento é desfeito. Este é o caso de quando não tem mais movimento para desfazer.*/
process_input('z', Map, OriginalMap, Marks, PlayerPos, [], Status) :- 
    write('Não é possível desfazer mais!'), nl,
    sleep(1),
    game_loop(Map, OriginalMap, Marks, PlayerPos, [], Status).

/**Se a entrada for 'z' o movimento é desfeito. Este é o caso de quando tem movimento para desfazer.*/
process_input('z', _, OriginalMap, Marks, _, [state(MapaAnterior, PosAnterior)|RestoHistorico], Status) :-
    game_loop(MapaAnterior, OriginalMap, Marks, PosAnterior, RestoHistorico, Status), !.

/**Lida com os movimentos e armazena o estado do jogo.*/
process_input(Move, Map, OriginalMap, Marks, PlayerPos, Historico, Status) :-
    move_player(Map, Marks, PlayerPos, Move, NovoMapa, NovoPlayerPos),
    ( PlayerPos \== NovoPlayerPos ->   
        NovoHistorico = [state(Map, PlayerPos) | Historico]
    ; 
        NovoHistorico = Historico
    ),
    game_loop(NovoMapa, OriginalMap, Marks, NovoPlayerPos, NovoHistorico, Status).


/**
Predicado que encontra a posicao do jogador no mapa.
@param Maps: O mapa do jogo.
@param Pos: A posicao do jogador, unificada com (Linha, Coluna).
*/
find_player(Map, (Row, Col)) :-
    nth0(Row, Map, Line),
    nth0(Col, Line, jogador), !.


/**
Predicado que verifica se uma coordenada é uma parede.
@param Mapa: O mapa atual do jogo.
@param Linha: O índice da linha a ser verificada.
@param Coluna: O índice da coluna a ser verificada.
*/
eh_parede(Map,Linha,Coluna) :-
    nth0(Linha, Map, Line),
    nth0(Coluna, Line, Cell),
    Cell == parede.

/**
Predicado que verifica se uma coordenada é uma caixa.
@param Mapa: O mapa atual do jogo.
@param Linha: O índice da linha a ser verificada.
@param Coluna: O índice da coluna a ser verificada.
*/
eh_caixa(Map,Linha,Coluna) :-
    nth0(Linha, Map, Line),
    nth0(Coluna, Line, Cell),
    Cell == caixa.

/**
Predicado que verifica se uma coordenada é uma marca(Alvo).
@param Marks: As marcas do mapa. 
@param Linha: O índice da linha a ser verificada.
@param Coluna: O índice da coluna a ser verificada.
*/
eh_marca(Marks, Linha, Coluna) :-
    member([Linha, Coluna], Marks).

/**
Fatos que definem as mudancas de coordenadas para cada movimento.
*/
delta(w, -1, 0).
delta(s, 1, 0).
delta(a, 0, -1).
delta(d, 0, 1).

/**
Predicado que move o jogador e lida com as colisoes.
@param Mapa: Mapa inicial.
@param Marks: Posicoes dos alvos.
@param PosJogador: Posicao inicial do jogador.
@param Move: O comando de movimento.
@param NovoMapa: O mapa apos o movimento.
@param NovaPosJogador: A nova posicao do jogador.
*/
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

/**
Predicado que checa se o jogo foi vencido.
@param Mapa: O mapa atual.
@param Marks: As posicoes dos alvos.
*/
checa_vitoria(Map, Marks) :-
    forall(
        member([Linha, Coluna], Marks),
        eh_caixa(Map, Linha, Coluna)
    ).


/**
Predicado responsável por criar um novo mapa com um valor atualizado em uma posicao especifica.
@param Mapa: O mapa original.
@param Posicao: A posicao a ser atualizada.
@param Valor: O novo valor para a posicao.
@param NovoMap: O novo mapa com o valor atualizado.
*/
update_map(Map, (Row, Col), Value, NewMap) :-
    nth0(Row, Map, OldLine, RestLines),
    nth0(Col, OldLine, _OldValue, RestCells),
    nth0(Col, NewLine, Value, RestCells),
    nth0(Row, NewMap, NewLine, RestLines).

/**
Predicado recursivo que imprime o mapa linha por linha.
*/
print_tile_map([]).
print_tile_map([Linha|Resto]) :-
    print_line(Linha),
    print_tile_map(Resto).

/**
Predicado recursivo que imprime cada celula de uma linha.
*/
print_line([]) :- nl.
print_line([Celula|Resto]) :-
    print_cell(Celula),
    print_line(Resto).

/**
Mapeamento dos nomes das celulas para os caracteres de exibicao.
*/
print_cell(parede)  :- write('█').
print_cell(vazio)   :- write(' ').
print_cell(jogador) :- write('@').
print_cell(caixa)   :- write('B').
print_cell(marca)   :- write('x').

/**
Predicado que imprime uma mensagem de vitoria.
*/
print_you_win :-
    write('VITÓRIA'), nl.

/**
Predicado que imprime as instruções de controle para o usuario.
*/
print_controls :-
    nl,
    write('Controles: [W,A,S,D] Movimentam | [Z] Desfazer | [R] Resetar | [M] Voltar ao Menu'), nl.

/**
Predicado que captura uma unica tecla pressionada pelo usuario e valida o comando.
@param Move: A variavel que sera unificada com o comando valido. 
*/
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