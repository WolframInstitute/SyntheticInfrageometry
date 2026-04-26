Package["WolframInstitute`SyntheticInfrageometry`"]


(* TorusTessellation[{n, m}, {p, q}] glues copies of CycleGraph[p] into
   a torus graph in which each vertex has q polygons meeting at it. We
   are not embedding in any Riemannian surface; this is a purely
   combinatorial gluing of cycle graphs.

   The pair (p, q) is the Schlafli symbol of a regular tessellation. For
   F polygons on a torus the Euler relation V - E + F = 0 with 2 E = p F
   and 2 E = q V forces 1/p + 1/q = 1/2. Over the integers this admits
   exactly three solutions: (3, 6), (4, 4), (6, 3) - the three flat-
   torus tessellations. Other (p, q) violate the Euler relation on a
   torus and are rejected; spherical (1/p + 1/q > 1/2) cases tile finite
   polyhedra and hyperbolic (1/p + 1/q < 1/2) cases tile higher-genus
   surfaces - both lie outside the torus and so outside this constructor.

   The argument {n, m} is the rectangular fundamental-domain grid; the
   resulting cell count depends on (p, q):

      (3, 6) triangles: F = 2 n m,  V = n m,    E = 3 n m
      (4, 4) squares:   F =   n m,  V = n m,    E = 2 n m
      (6, 3) hexagons:  F =   n m,  V = 2 n m,  E = 3 n m

   Each output is connected and vertex-transitive, so any local-
   isomorphism-invariant graph quantity (ball volume, Wolfram-Hausdorff
   dimension, Wolfram-Ricci curvature based on volume growth) is
   pointwise constant.

   The integer-only form TorusTessellation[{n, m}, p] is shorthand for
   TorusTessellation[{n, m}, {p, 2 p / (p - 2)}], i.e. q is read off the
   Euclidean valency formula. *)


TorusTessellation::nontorus =
  "Schlafli symbol {`1`, `2`} does not satisfy the flat-torus Euler \
relation 1/p + 1/q = 1/2; only {3, 6}, {4, 4}, {6, 3} are realised. \
Spherical cases ({3,3}, {3,4}, {4,3}, {3,5}, {5,3}) tile finite \
polyhedra; hyperbolic cases (e.g. {5,4}, {7,3}) tile higher-genus \
surfaces.";


(* {3, 6}: triangular lattice on an n x m rectangular grid of vertices,
   each fundamental rectangle split into 2 triangles by the diagonal
   {i,j}-{i+1,j+1}. *)

TorusTessellation[ { n_Integer, m_Integer }, { 3, 6 } ] :=
  With[ { wrap = { ii, jj } |-> { Mod[ ii - 1, n ] + 1, Mod[ jj - 1, m ] + 1 } },
    Graph[
      Flatten[ Table[ { i, j }, { i, n }, { j, m } ], 1 ],
      Flatten @ Table[
        { { i, j } <-> wrap[ i + 1, j ],
          { i, j } <-> wrap[ i, j + 1 ],
          { i, j } <-> wrap[ i + 1, j + 1 ] },
        { i, n }, { j, m }
      ]
    ]
  ]


(* {4, 4}: square lattice on an n x m grid of vertices. *)

TorusTessellation[ { n_Integer, m_Integer }, { 4, 4 } ] :=
  With[ { wrap = { ii, jj } |-> { Mod[ ii - 1, n ] + 1, Mod[ jj - 1, m ] + 1 } },
    Graph[
      Flatten[ Table[ { i, j }, { i, n }, { j, m } ], 1 ],
      Flatten @ Table[
        { { i, j } <-> wrap[ i + 1, j ],
          { i, j } <-> wrap[ i, j + 1 ] },
        { i, n }, { j, m }
      ]
    ]
  ]


(* {6, 3}: A/B sublattice honeycomb on an n x m grid of unit cells, each
   unit cell carrying 2 vertices. *)

TorusTessellation[ { n_Integer, m_Integer }, { 6, 3 } ] :=
  With[ {
    a = { ii, jj } |-> { "A", Mod[ ii - 1, n ] + 1, Mod[ jj - 1, m ] + 1 },
    b = { ii, jj } |-> { "B", Mod[ ii - 1, n ] + 1, Mod[ jj - 1, m ] + 1 }
  },
    Graph[
      Join[
        Flatten[ Table[ a[ i, j ], { i, n }, { j, m } ], 1 ],
        Flatten[ Table[ b[ i, j ], { i, n }, { j, m } ], 1 ]
      ],
      Flatten @ Table[
        { a[ i, j ] <-> b[ i, j ],
          a[ i, j ] <-> b[ i - 1, j ],
          a[ i, j ] <-> b[ i, j - 1 ] },
        { i, n }, { j, m }
      ]
    ]
  ]


(* Reject every other Schlafli pair with the Euler explanation. *)

TorusTessellation[ { _Integer, _Integer }, { p_Integer, q_Integer } ] /;
    p >= 3 && q >= 3 :=
  ( Message[ TorusTessellation::nontorus, p, q ]; $Failed )


(* Shorthand: integer p reads q off the Euclidean valency formula
   q = 2 p / (p - 2). When q is not an integer the resulting Schlafli
   pair is not flat-torus; the dispatch above raises the message. *)

TorusTessellation[ { n_Integer, m_Integer }, p_Integer ] /; p >= 3 :=
  With[ { q = 2 p / ( p - 2 ) },
    If[ IntegerQ[ q ],
      TorusTessellation[ { n, m }, { p, q } ],
      Message[ TorusTessellation::nontorus, p, q ]; $Failed
    ]
  ]


(* Single-int alias: square fundamental-domain grid. *)

TorusTessellation[ n_Integer, k_ ] := TorusTessellation[ { n, n }, k ]
