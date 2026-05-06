Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Example graphs ===================== *)

(* InfraExampleGraph[name, params, opts] is the paclet's example-graph
   registry, modelled on ExampleData.  A single addressable family used by
   guides, tutorials, and symbol-page demonstrations as a canonical running
   graph.  Graphs cover the curvature spectrum (flat lattice / Euclidean mesh
   / negative-curvature tree / positive-curvature spherical mesh) plus
   standard small named gems and Cayley graphs.

   Calling forms:
     InfraExampleGraph[]                           -- list of available keys
     InfraExampleGraph[name]                       -- default parameters
     InfraExampleGraph[name, params, opts...]      -- with parameters
*)

$InfraExampleGraphRegistry = <|
  "Grid"              -> { { 4, 4 }                  , "GridGraph rectangular lattice (4-regular interior)." },
  "RectangleMesh"     -> { { 3., 2. }                , "Triangulated rectangle [0,a]x[0,b] via DiscretizeRegion." },
  "DiskMesh"          -> { 1.                        , "Triangulated disk of radius r (mesh boundary effects mimic negative curvature)." },
  "SphereMesh"        -> { 1.                        , "Triangulated sphere of radius r (positive curvature)." },
  "TriangularLattice" -> { 5                         , "Triangular lattice of side n (6-regular interior)." },
  "HexagonalLattice"  -> { { 3, 3, 3 }               , "Hexagonal lattice with row/column/diagonal parameters." },
  "RegularTree"       -> { { 3, 3 }                  , "Tree of degree d and depth h (root has d leaves, others have d-1 children)." },
  "Cayley"            -> { { "SymmetricGroup", 4 }   , "Cayley graph of a named finite group (FiniteGroupData)." },
  "Petersen"          -> { None                      , "Petersen graph: 3-regular, girth 5, distance-regular." },
  "Heawood"           -> { None                      , "Heawood graph: 3-regular bipartite, girth 6, point-line incidence of PG(2, 2)." },
  "MobiusKantor"      -> { None                      , "Moebius-Kantor graph: 3-regular, girth 6, generalized Petersen GP(8, 3)." },
  "Tutte"             -> { None                      , "Tutte 12-cage: 3-regular, girth 12." }
|>;


InfraExampleGraph[ ] :=
  Keys @ $InfraExampleGraphRegistry

InfraExampleGraph[ name_String, opts : OptionsPattern[ ] ] :=
  InfraExampleGraph[ name, $InfraExampleGraphRegistry[ name ][[ 1 ]], opts ]

InfraExampleGraph[ "Grid", { m_Integer, n_Integer }, opts : OptionsPattern[ ] ] :=
  GridGraph[ { m, n }, opts ]

InfraExampleGraph[ "RectangleMesh", { a_, b_ }, opts : OptionsPattern[ ] ] :=
  meshGraph[ Rectangle[ { 0, 0 }, { a, b } ], { opts }, 0.1 ]

InfraExampleGraph[ "DiskMesh", r_, opts : OptionsPattern[ ] ] :=
  meshGraph[ Disk[ { 0, 0 }, r ], { opts }, 0.05 ]

InfraExampleGraph[ "SphereMesh", r_, opts : OptionsPattern[ ] ] :=
  meshGraph[ Sphere[ { 0, 0, 0 }, r ], { opts }, 0.05 ]


meshGraph[ region_, opts_List, defaultMaxCell_ ] :=
  With[ { mesh = DiscretizeRegion[ region,
      MaxCellMeasure -> Replace[ MaxCellMeasure /. opts, MaxCellMeasure -> defaultMaxCell ],
      Sequence @@ FilterRules[ opts, Except[ MaxCellMeasure ] ] ] },
    MeshConnectivityGraph[ mesh, 0 ]
  ]

InfraExampleGraph[ "TriangularLattice", n_Integer, opts : OptionsPattern[ ] ] :=
  Graph[ GraphData[ { "Triangular", n } ], opts ]

InfraExampleGraph[ "HexagonalLattice", { a_Integer, b_Integer, c_Integer }, opts : OptionsPattern[ ] ] :=
  Graph[ GraphData[ { "HexagonalGrid", { a, b, c } } ], opts ]

InfraExampleGraph[ "RegularTree", { degree_Integer, depth_Integer }, opts : OptionsPattern[ ] ] :=
  Graph[ regularTreeEdges[ degree, depth ], opts, GraphLayout -> "RadialEmbedding" ]

InfraExampleGraph[ "Cayley", group_, opts : OptionsPattern[ ] ] :=
  Graph[ CayleyGraph[ FiniteGroupData[ group, "PermutationGroupRepresentation" ] ], opts ]

InfraExampleGraph[ "Petersen", None, opts : OptionsPattern[ ] ] :=
  Graph[ PetersenGraph[ ], opts ]

InfraExampleGraph[ "Heawood", None, opts : OptionsPattern[ ] ] :=
  Graph[ GraphData[ "HeawoodGraph" ], opts ]

InfraExampleGraph[ "MobiusKantor", None, opts : OptionsPattern[ ] ] :=
  Graph[ GraphData[ "MoebiusKantorGraph" ], opts ]

InfraExampleGraph[ "Tutte", None, opts : OptionsPattern[ ] ] :=
  Graph[ GraphData[ "Tutte12Cage" ], opts ]


(* regularTreeEdges[degree, depth] enumerates the edge list of a rooted tree
   in which the root has `degree` children and every internal vertex has
   `degree - 1` children, of total depth `depth`.  Vertices are integers
   1, 2, 3, ... in breadth-first order. *)

regularTreeEdges[ degree_Integer, depth_Integer ] :=
  Module[ { edges = { }, frontier = { 1 }, next, idx = 1, level },
    Do[
      next = { };
      Do[
        With[ { branching = If[ level == 1, degree, degree - 1 ] },
          Do[ idx++; AppendTo[ edges, parent <-> idx ]; AppendTo[ next, idx ],
              { branching } ]
        ],
        { parent, frontier }
      ];
      frontier = next,
      { level, depth }
    ];
    edges
  ]
