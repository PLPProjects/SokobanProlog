:- module(facil, [mapas/1]).

mapas([
  mapa(
        [ [parede, parede, parede, parede, parede, parede, parede],
          [parede, jogador, vazio, vazio, vazio, parede, parede],
          [parede, vazio, caixa, caixa, vazio, vazio, parede],
          [parede, vazio, parede, marca, vazio, marca, parede],
          [parede, vazio, vazio, vazio, vazio, vazio, parede],
          [parede, parede, parede, parede, parede, parede, parede] ],
        [[3,3],[3,5]],
        facil
    ),

  mapa(
        [ [parede, parede, parede, parede, parede, parede, parede],
          [parede, parede, parede, vazio, vazio, parede, parede],
          [parede, jogador, vazio, marca, caixa, parede, parede],
          [parede, vazio, vazio, vazio, caixa, vazio, parede],
          [parede, vazio, parede, marca, vazio, vazio, parede],
          [parede, vazio, vazio, vazio, vazio, vazio, parede],
          [parede, parede, parede, parede, parede, parede, parede] ],
        [[2,3],[4,3]],
        facil
    ),

  mapa(
        [ [parede, parede, parede, parede, parede, parede, parede, vazio],
          [parede, vazio, vazio, marca, vazio, vazio, parede, parede],
          [parede, vazio, caixa, caixa, parede, marca, vazio, parede],
          [parede, vazio, parede, jogador, caixa, vazio, vazio, parede],
          [parede, vazio, vazio, marca, parede, vazio, vazio, parede],
          [parede, parede, parede, parede, parede, parede, parede, parede] ],
        [[1,3],[2,5],[4,3]],
        facil
    ),

  mapa(
        [ [parede, parede, parede, parede, parede, parede, parede],
          [parede, jogador, vazio, vazio, parede, parede, parede],
          [parede, vazio, caixa, caixa, caixa, vazio, parede],
          [parede, parede, vazio, marca, marca, marca, parede],
          [parede, vazio, vazio, vazio, parede, vazio, parede],
          [parede, vazio, vazio, vazio, vazio, vazio, parede],
          [parede, parede, parede, parede, parede, parede, parede] ],
        [[3,3],[3,4],[3,5]],
        facil
    ),

  mapa(
        [ [parede, parede, parede, parede, parede, parede, parede, parede],
          [parede, vazio, vazio, vazio, vazio, parede, parede, parede],
          [parede, vazio, vazio, vazio, marca, marca, vazio, parede],
          [parede, vazio, caixa, caixa, caixa, jogador, vazio, parede],
          [parede, vazio, vazio, parede, vazio, marca, vazio, parede],
          [parede, parede, parede, parede, parede, parede, parede, parede] ],
        [[2,4],[2,5],[4,5]],
        facil
    )

]).
