:- module(medio, [maps/1]).

maps([

    map(
        [ [vazio,vazio,vazio,vazio,parede,parede,parede,parede,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [vazio,vazio,vazio,vazio,parede,vazio,vazio,vazio,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [vazio,vazio,vazio,vazio,parede,caixa,vazio,vazio,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [vazio,vazio,parede,parede,parede,vazio,vazio,caixa,parede,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [vazio,vazio,parede,vazio,vazio,caixa,vazio,caixa,vazio,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [parede,parede,parede,vazio,parede,vazio,parede,parede,vazio,parede,vazio,vazio,vazio,parede,parede,parede,parede,parede,parede],
          [parede,vazio,vazio,vazio,parede,vazio,parede,parede,vazio,parede,parede,parede,parede,vazio,marca,marca,parede],
          [parede,vazio,caixa,vazio,vazio,caixa,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,marca,marca,parede],
          [parede,parede,parede,parede,parede,vazio,parede,vazio,vazio,vazio,parede,parede,jogador,parede,vazio,marca,marca,parede],
          [vazio,vazio,vazio,vazio,parede,vazio,vazio,vazio,vazio,vazio,parede,parede,parede,parede,parede,parede,parede],
          [vazio,vazio,vazio,vazio,parede,parede,parede,parede,parede,parede,parede,vazio,vazio,vazio,vazio,vazio,vazio]
        ],
        [[6,16],[6,17],[7,16],[7,17],[8,16],[8,17]],
        medio
    ),

    map(
        [ [parede,parede,parede,parede,parede,parede,parede,parede,parede,parede,parede,parede,vazio,vazio],
          [parede,marca,marca,vazio,vazio,parede,vazio,vazio,vazio,vazio,vazio,parede,parede,parede],
          [parede,marca,marca,vazio,parede,vazio,caixa,vazio,vazio,caixa,vazio,vazio,vazio,parede],
          [parede,marca,marca,vazio,parede,caixa,parede,parede,parede,parede,vazio,vazio,vazio,parede],
          [parede,marca,marca,vazio,vazio,vazio,jogador,vazio,parede,parede,vazio,vazio,vazio,parede],
          [parede,marca,marca,vazio,parede,vazio,parede,vazio,caixa,vazio,vazio,parede,parede],
          [parede,parede,parede,parede,parede,parede,vazio,parede,parede,caixa,vazio,caixa,vazio,parede],
          [vazio,vazio,parede,vazio,caixa,vazio,caixa,vazio,caixa,vazio,caixa,vazio,parede],
          [vazio,vazio,parede,vazio,vazio,vazio,parede,vazio,vazio,vazio,vazio,parede],
          [vazio,vazio,parede,parede,parede,parede,parede,parede,parede,parede,parede,parede]
        ],
        [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2],[4,1],[4,2],[5,1],[5,2]],
        medio
    ),

    map(
        [ [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,parede,parede,parede,parede,parede,vazio],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,vazio,vazio,vazio,jogador,parede,vazio],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,caixa,parede,caixa,vazio,parede,parede,vazio],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,caixa,vazio,vazio,caixa,parede,vazio,vazio],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,parede,caixa,vazio,caixa,vazio,parede,vazio,vazio],
          [parede,parede,parede,parede,parede,parede,parede,parede,parede,vazio,caixa,vazio,parede,vazio,parede,parede,parede],
          [parede,marca,marca,marca,vazio,vazio,parede,parede,caixa,vazio,vazio,caixa,vazio,vazio,vazio,parede],
          [parede,parede,marca,marca,marca,vazio,vazio,vazio,vazio,caixa,vazio,vazio,vazio,vazio,vazio,parede],
          [parede,marca,marca,marca,vazio,vazio,parede,parede,parede,parede,parede,parede,parede,parede,parede,parede],
          [parede,parede,parede,parede,parede,parede,parede,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio]
        ],
        [[6,1],[6,2],[6,3],[6,4],[7,2],[7,3],[7,4],[8,1],[8,2],[8,3],[8,4]],
        medio
    ),

    map(
        [ [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,parede,parede,parede,parede,parede,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,vazio,marca,marca,marca,parede],
          [parede,parede,parede,parede,parede,parede,parede,parede,parede,parede,vazio,vazio,marca,marca,marca,parede],
          [parede,vazio,vazio,vazio,parede,vazio,caixa,vazio,caixa,vazio,vazio,marca,marca,marca,parede],
          [parede,vazio,caixa,vazio,caixa,parede,caixa,vazio,vazio,parede,vazio,marca,marca,marca,parede],
          [parede,vazio,vazio,caixa,vazio,vazio,vazio,caixa,vazio,parede,vazio,marca,marca,marca,parede],
          [parede,vazio,caixa,caixa,vazio,parede,caixa,vazio,caixa,vazio,caixa,parede,parede,parede,parede,parede,parede,parede],
          [parede,vazio,vazio,caixa,vazio,vazio,vazio,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [parede,parede,vazio,parede,parede,parede,parede,parede,parede,parede,parede,vazio,vazio,vazio,vazio,vazio,vazio],
          [parede,vazio,vazio,vazio,parede,vazio,vazio,parede,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio],
          [parede,vazio,vazio,vazio,vazio,caixa,vazio,vazio,parede,parede,vazio,vazio,vazio,vazio,vazio,vazio],
          [parede,vazio,vazio,caixa,caixa,parede,caixa,caixa,vazio,vazio,jogador,parede,vazio,vazio,vazio,vazio],
          [parede,vazio,vazio,vazio,parede,vazio,vazio,vazio,parede,parede,vazio,vazio,vazio,vazio,vazio],
          [parede,parede,parede,parede,parede,parede,parede,parede,parede,parede,parede,vazio,vazio,vazio,vazio]
        ],
        [[1,14],[1,15],[1,16],[1,17],[2,14],[2,15],[2,16],[2,17],[3,14],[3,15],[3,16],[3,17],[4,14],[4,15],[4,16],[4,17],[5,14],[5,15],[5,16],[5,17]],
        medio
    ),

    map(
        [ [vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,parede,parede,parede,vazio,vazio],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,vazio,parede,parede,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,parede,caixa,parede,vazio,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,vazio,vazio,vazio,caixa,vazio,parede],
          [parede,parede,parede,parede,parede,parede,parede,parede,parede,vazio,parede,vazio,vazio,vazio,parede],
          [parede,marca,marca,marca,vazio,vazio,parede,parede,caixa,vazio,caixa,vazio,parede,parede],
          [parede,marca,marca,marca,vazio,vazio,vazio,caixa,caixa,vazio,parede,parede],
          [parede,marca,marca,marca,vazio,vazio,parede,caixa,vazio,vazio,jogador,parede],
          [parede,parede,parede,parede,parede,parede,parede,parede,vazio,caixa,vazio,parede,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,caixa,vazio,vazio,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,parede,parede,vazio,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,vazio,vazio,parede],
          [vazio,vazio,vazio,vazio,vazio,vazio,vazio,vazio,parede,parede,parede,parede]
        ],
        [[5,1],[5,2],[5,3],[5,4],[6,1],[6,2],[6,3],[6,4],[7,1],[7,2],[7,3],[7,4]],
        medio
    )

]).
