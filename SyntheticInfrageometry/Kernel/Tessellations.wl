Package["WolframInstitute`SyntheticInfrageometry`"]


(* TorusTessellation[{n, m}, p] glues copies of CycleGraph[p] into a
   torus graph along the rectangular fundamental-domain grid {n, m}.
   This is a purely combinatorial gluing of cycle graphs - we are not
   embedding in any Riemannian surface.

   The polygon size p must be 3, 4, or 6: these are the only integer
   solutions of the flat-torus Euler relation 1/p + 1/q = 1/2 (where
   q = 2 p / (p - 2) is the number of polygons meeting at each vertex,
   read off the regular {p, q} tessellation). Pentagons, heptagons and
   the like require non-flat substrates; see SchlafliTessellation
   (placeholder) and Wiki/Plans/SchlafliGluings.md.

   The (n, m) input is the rectangular fundamental-domain grid; the
   resulting cell count depends on p:

      p = 3 (triangles): V = n m,   E = 3 n m,  F = 2 n m,  valency 6
      p = 4 (squares):   V = n m,   E = 2 n m,  F =   n m,  valency 4
      p = 6 (hexagons):  V = 2 n m, E = 3 n m,  F =   n m,  valency 3

   Each output is connected and vertex-transitive, so any local-
   isomorphism-invariant graph quantity (ball volume, Wolfram-Hausdorff
   dimension, Wolfram-Ricci curvature based on volume growth) is
   pointwise constant. *)


TorusTessellation::badp =
  "Polygon size `1` does not yield a flat-torus tessellation: q = 2 p / (p - 2) = `2` is not an integer. Pentagons, heptagons and the like tile spherical or hyperbolic surfaces and need the (placeholder) SchlafliTessellation generator instead.";


(* p = 3: triangular lattice on the n x m vertex grid, each
   fundamental rectangle split into 2 triangles by the diagonal. *)

TorusTessellation[ { n_Integer, m_Integer }, 3 ] :=
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


(* p = 4: square lattice on the n x m vertex grid. *)

TorusTessellation[ { n_Integer, m_Integer }, 4 ] :=
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


(* p = 6: A/B sublattice honeycomb on the n x m unit-cell grid. *)

TorusTessellation[ { n_Integer, m_Integer }, 6 ] :=
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


(* Reject other p with the Euler explanation. *)

TorusTessellation[ { _Integer, _Integer }, p_Integer ] /; p >= 3 :=
  ( Message[ TorusTessellation::badp, p, 2 p / ( p - 2 ) ]; $Failed )


(* Single-int alias: square fundamental-domain grid. *)

TorusTessellation[ n_Integer, p_Integer ] := TorusTessellation[ { n, n }, p ]


(* SchlafliTessellation[{p, q}, ...] is a placeholder for the general
   gluing of CycleGraph[p] copies into a graph in which q polygons meet
   at each vertex, for any Schlafli pair {p, q} (not only the three
   flat-torus ones). The construction is well-defined in principle:

      - 1/p + 1/q > 1/2 (spherical):  the unique Platonic solid graph
                                      ({3,3} = tetrahedron, {3,4} =
                                      octahedron, {4,3} = cube, {3,5}
                                      = icosahedron, {5,3} = dodecahedron)
      - 1/p + 1/q = 1/2 (Euclidean):  forwards to TorusTessellation
      - 1/p + 1/q < 1/2 (hyperbolic): finite-index torsion-free quotient
                                      of the {p, q} triangle group; the
                                      genuine open work

   Not yet implemented. See Wiki/Plans/SchlafliGluings.md. *)

SchlafliTessellation::nyi =
  "SchlafliTessellation[{p, q}, ...] is a placeholder for the general regular gluing of CycleGraph[p] copies; not yet implemented. See Wiki/Plans/SchlafliGluings.md. For the three Euclidean cases use TorusTessellation[{n, m}, p] with p in {3, 4, 6} directly.";

SchlafliTessellation[ ___ ] :=
  ( Message[ SchlafliTessellation::nyi ]; $Failed )
