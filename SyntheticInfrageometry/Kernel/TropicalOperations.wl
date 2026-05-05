Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Tropical (min-plus) operations ===================== *)

(* Min-plus semiring: tropical addition is Min (identity \[Infinity]),
   tropical multiplication is Plus (identity 0).  The two scalar heads
   are Listable, so they extend element-wise to vectors and matrices.
   TropicalDot is matrix/vector product in the semiring,
   Inner[ Plus, A, B, Min ], and TropicalMatrixPower is its iterate;
   TropicalMatrixPower[ A, 0 ] is the tropical identity (0 on the
   diagonal, \[Infinity] elsewhere).

   Scope: pure min-plus arithmetic on scalars / vectors / matrices.
   The matrices in this paclet typically come from a graph (the cost
   adjacency or the distance matrix), but these operations are agnostic
   about that -- they are sense-1 tropical (arithmetic), not sense-2
   tropical (graph-translated convexity). *)

SetAttributes[ TropicalPlus, Listable ]
TropicalPlus[ args___ ] := Min[ args ]

SetAttributes[ TropicalTimes, Listable ]
TropicalTimes[ args___ ] := Plus[ args ]

TropicalDot[ A_, B_ ] := Inner[ Plus, A, B, Min ]

TropicalMatrixPower[ A_?SquareMatrixQ, 0 ] :=
  With[ { n = Length[ A ] },
    Table[ If[ i == j, 0, Infinity ], { i, n }, { j, n } ]
  ]
TropicalMatrixPower[ A_?MatrixQ, 1 ] := A
TropicalMatrixPower[ A_?MatrixQ, k_Integer /; k > 1 ] :=
  Nest[ TropicalDot[ #, A ] &, A, k - 1 ]
