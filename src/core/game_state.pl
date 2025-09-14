/**
@file game_state.pl: Modulo principal que gerencia a lógica do jogo.
Responsável pelo loop principal, movimentos, checagem de vitória e manipulação do estado do jogo.
*/

/** 
Declara o arquivo como um módulo e exporta o predicado iniciar para os outros modulos.
*/
:- module(game_state, [
        iniciar/2
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
iniciar(Mapa, Status) :- 
    Mapa = mapa(TileMap, Marcas, _Dificuldade),
    encontrar_jogador(TileMap, PosJogador),
    loop_jogo(TileMap, Mapa, Marcas, PosJogador, [], Status).


/**
Predicado que inicia o loop principal do jogo.
@param Mapa: A representacao atual do tabuleiro.
@param MapaOriginal: O mapa original.
@param Marcas: As posicoes dos alvos.
@param PosJogador: A posicao atual do jogador.
@param Historico: A lista de estados anteriores para desfazer movimentos.
@param Status: A variavel que sera unificada com o status final do jogo.
*/
loop_jogo(Mapa, MapaOriginal, Marcas, PosJogador, Historico, Status) :-
    console:limpar_tela,
    nl,
    imprimir_mapa(Mapa),
    imprimir_controles,
    ( checar_vitoria(Mapa, Marcas) ->
        imprimir_vitoria,
        Status = vitoria,
        !
    ;
        capturar_movimento(Movimento),
        processar_entrada(Movimento, Mapa, MapaOriginal, Marcas, PosJogador, Historico, Status)
    ).


/**
Predicado que processa a entrada do usuario.
@param Movimento: O comando de movimento.
@param Mapa, MapaOriginal, Marcas, PosJogador, Historico: Dados do estado do jogo.
@param Status: Status do jogo.
*/
/**Se a entrada for 'm' volta para o menu. */
processar_entrada('m', _, _, _, _, _, voltou_menu) :- !.

/**Se a entrada for 'r' o jogo é resetado. */
processar_entrada('r', _, MapaOriginal, _, _, _, Status) :- 
    iniciar(MapaOriginal, Status), !.

/**Se a entrada for 'z' o movimento é desfeito. Este é o caso de quando não tem mais movimento para desfazer.*/
processar_entrada('z', Mapa, MapaOriginal, Marcas, PosJogador, [], Status) :- 
    write('\e[31mNão é possível desfazer mais!\e[0m'), nl,
    sleep(1),
    loop_jogo(Mapa, MapaOriginal, Marcas, PosJogador, [], Status).      

/**Se a entrada for 'z' o movimento é desfeito. Este é o caso de quando tem movimento para desfazer.*/
processar_entrada('z', _, MapaOriginal, Marcas, _, [estado(MapaAnterior, PosAnterior)|RestoHistorico], Status) :-
    loop_jogo(MapaAnterior, MapaOriginal, Marcas, PosAnterior, RestoHistorico, Status), !.

/**Lida com os movimentos e armazena o estado do jogo.*/
processar_entrada(Movimento, Mapa, MapaOriginal, Marcas, PosJogador, Historico, Status) :-
    mover_jogador(Mapa, Marcas, PosJogador, Movimento, NovoMapa, NovaPosJogador),
    ( PosJogador \== NovaPosJogador ->   
        NovoHistorico = [estado(Mapa, PosJogador) | Historico]
    ; 
        NovoHistorico = Historico
    ),
    loop_jogo(NovoMapa, MapaOriginal, Marcas, NovaPosJogador, NovoHistorico, Status).


/**
Predicado que encontra a posicao do jogador no mapa.
@param Mapa: O mapa do jogo.
@param Pos: A posicao do jogador, unificada com (Linha, Coluna).
*/
encontrar_jogador(Mapa, (Linha, Coluna)) :-
    nth0(Linha, Mapa, LinhaMapa),
    nth0(Coluna, LinhaMapa, jogador), !.


/**
Predicado que verifica se uma coordenada é uma parede.
@param Mapa: O mapa atual do jogo.
@param Linha: O índice da linha a ser verificada.
@param Coluna: O índice da coluna a ser verificada.
*/
eh_parede(Mapa,Linha,Coluna) :-
    nth0(Linha, Mapa, LinhaMapa),
    nth0(Coluna, LinhaMapa, Celula),
    Celula == parede.


/**
Predicado que verifica se uma coordenada é uma caixa.
@param Mapa: O mapa atual do jogo.
@param Linha: O índice da linha a ser verificada.
@param Coluna: O índice da coluna a ser verificada.
*/
eh_caixa(Mapa,Linha,Coluna) :-
    nth0(Linha, Mapa, LinhaMapa),
    nth0(Coluna, LinhaMapa, Celula),
    Celula == caixa.


/**
Predicado que verifica se uma coordenada é uma marca(Alvo).
@param Marcas: As marcas do mapa. 
@param Linha: O índice da linha a ser verificada.
@param Coluna: O índice da coluna a ser verificada.
*/
eh_marca(Marcas, Linha, Coluna) :-
    member([Linha, Coluna], Marcas).


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
@param Marcas: Posicoes dos alvos.
@param PosJogador: Posicao inicial do jogador.
@param Movimento: O comando de movimento.
@param NovoMapa: O mapa apos o movimento.
@param NovaPosJogador: A nova posicao do jogador.
*/
mover_jogador(Mapa, Marcas, (Linha, Coluna), Movimento, NovoMapa, NovaPosJogador) :-
    delta(Movimento, DY, DX),
    NovaLinha is Linha + DY,
    NovaColuna is Coluna + DX,

    (\+ eh_parede(Mapa, NovaLinha, NovaColuna) ->
        (eh_caixa(Mapa, NovaLinha, NovaColuna) ->
            Linha_depois_caixa is NovaLinha + DY,
            Coluna_depois_caixa is NovaColuna + DX,
            (\+eh_caixa(Mapa, Linha_depois_caixa, Coluna_depois_caixa),\+eh_parede(Mapa, Linha_depois_caixa, Coluna_depois_caixa) ->
                    (eh_marca(Marcas, Linha, Coluna) -> 
                        atualizar_mapa(Mapa, (Linha, Coluna), marca, TempMapa)
                    ;
                        atualizar_mapa(Mapa, (Linha, Coluna), vazio, TempMapa)
                    ),
                    atualizar_mapa(TempMapa, (NovaLinha, NovaColuna), jogador, TempMapa2),
                    atualizar_mapa(TempMapa2, (Linha_depois_caixa, Coluna_depois_caixa), caixa, NovoMapa),
                    NovaPosJogador = (NovaLinha, NovaColuna)
                ;
                    NovoMapa = Mapa,
                    NovaPosJogador = (Linha, Coluna)
                )
            ;
            (eh_marca(Marcas, Linha, Coluna) -> 
                atualizar_mapa(Mapa, (Linha, Coluna), marca, TempMapa)
            ;
                atualizar_mapa(Mapa, (Linha, Coluna), vazio, TempMapa)
            ),
            atualizar_mapa(TempMapa, (NovaLinha, NovaColuna), jogador, NovoMapa),
            NovaPosJogador = (NovaLinha, NovaColuna)
            )
        ;
        NovoMapa = Mapa,
        NovaPosJogador = (Linha, Coluna)
        ).

/**
Predicado que checa se o jogo foi vencido.
@param Mapa: O mapa atual.
@param Marcas: As posicoes dos alvos.
*/
checar_vitoria(Mapa, Marcas) :-
    forall(
    member([Linha, Coluna], Marcas),
    eh_caixa(Mapa, Linha, Coluna)
    ).


/**
Predicado responsável por criar um novo mapa com um valor atualizado em uma posicao especifica.
@param Mapa: O mapa original.
@param Posicao: A posicao a ser atualizada.
@param Valor: O novo valor para a posicao.
@param NovoMap: O novo mapa com o valor atualizado.
*/

atualizar_mapa(Mapa, (Linha, Coluna), Valor, NovoMapa) :-
    nth0(Linha, Mapa, LinhaAntiga, RestoLinhas),
    nth0(Coluna, LinhaAntiga, _ValorAntigo, RestoCelulas),
    nth0(Coluna, NovaLinha, Valor, RestoCelulas),
    nth0(Linha, NovoMapa, NovaLinha, RestoLinhas).


/**
Predicado recursivo que imprime o mapa linha por linha.
*/
imprimir_mapa([]).
imprimir_mapa([Linha|Resto]) :-
    imprimir_linha(Linha),
    imprimir_mapa(Resto).


/**
Predicado recursivo que imprime cada celula de uma linha.
*/
imprimir_linha([]) :- nl.
imprimir_linha([Celula|Resto]) :-
    imprimir_celula(Celula),
    imprimir_linha(Resto).


/**
Mapeamento dos nomes das celulas para os caracteres de exibicao.
*/
imprimir_celula(parede)  :- write('🟫').
imprimir_celula(vazio)   :- write('⬛').
imprimir_celula(jogador) :- write('😀').
imprimir_celula(caixa)   :- write('📦').
imprimir_celula(marca)   :- write('🎯').



/**
Predicado que imprime uma mensagem de vitoria.
*/
imprimir_vitoria :-
    write('\e[32mVITÓRIA \e[0m'), nl.



/**
Predicado que imprime as instruções de controle para o usuario.
*/
imprimir_controles :-
    nl,
    write('Controles: [W,A,S,D] Mover | [Z] Desfazer | [R] Reiniciar | [M] Voltar ao Menu'), nl.


/**
Predicado que captura uma unica tecla pressionada pelo usuario e valida o comando.
@param Move: A variavel que sera unificada com o comando valido. 
*/
capturar_movimento(Movimento) :-
        flush_output,
        get_single_char(Codigo),
        char_code(Caractere, Codigo),
        downcase_atom(Caractere, Minusculo),
        ( member(Minusculo, [w,a,s,d,r,m,z]) ->
                Movimento = Minusculo
        ; 
            capturar_movimento(Movimento)
        ).