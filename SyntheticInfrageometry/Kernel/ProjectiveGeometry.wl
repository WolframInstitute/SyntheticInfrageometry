Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Pointwise predicates ===================== *)

(* SameDirectionQ[g, O, v, w]: v and w lie in the same pencil-direction
   at O - some maximal geodesic through O contains both. *)

SameDirectionQ[ graph_Graph, O_, v_, w_ ] :=
  v === w || AnyTrue[ FindLine[ graph, O, v, All ], MemberQ[ #[[ 1, 1 ]], w ] & ]


(* CollinearQ: there exists a single canonical line containing every
   listed vertex (the synthetic-incidence "lie on a common line"). *)

CollinearQ[ graph_Graph, verts_List ] :=
  Length[ DeleteDuplicates @ verts ] <= 1 ||
    Length @ FindCommonLine[ graph, verts, UpTo[ 1 ] ] > 0


(* ConcurrentQ: a set of lines shares a common vertex - the dual of
   collinearity. *)

ConcurrentQ[ graph_Graph, lines_List ] :=
  Length[ lines ] <= 1 ||
    Length @ FindCommonPoint[ graph, lines, UpTo[ 1 ] ] > 0


(* UniquePencilQ[g, O]: every direction at O is single-valued - exactly
   one maximal geodesic through O ending at v, for every v != O.        *)

UniquePencilQ[ graph_Graph, O_ ] :=
  AllTrue[ DeleteCases[ VertexList[ graph ], O ],
    Length @ FindLine[ graph, O, #, All ] == 1 & ]


(* UniqueCollinearQ: exactly one canonical line contains every listed
   vertex; the unique-witness companion of CollinearQ. *)

UniqueCollinearQ[ graph_Graph, verts_List ] :=
  Length @ FindCommonLine[ graph, verts, All ] == 1


(* UniqueConcurrentQ: the listed lines share exactly one common vertex;
   the unique-witness companion of ConcurrentQ. *)

UniqueConcurrentQ[ graph_Graph, lines_List ] :=
  Length @ FindCommonPoint[ graph, lines, All ] == 1


(* ===================== Whitehead axioms ===================== *)

(* W1: every line has at least three points.
   W2: through any two distinct vertices passes exactly one line.
   W3: if some line through {A, B} meets some line through {C, D}, then
       some line through {A, C} meets some line through {B, D}.        *)

WhiteheadW1Q[ graph_Graph ] :=
  AllTrue[ allCanonicalLines[ graph ], Length[ # ] >= 3 & ]

WhiteheadW2Q[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    UniqueCollinearQ[ graph, # ] & ]

WhiteheadW3Q[ graph_Graph ] :=
  Module[ { verts },
    verts = VertexList[ graph ];
    AllTrue[ Tuples[ verts, 4 ],
      abcd |-> If[ Length @ DeleteDuplicates @ abcd < 4, True,
        With[ { A = abcd[[ 1 ]], B = abcd[[ 2 ]], C = abcd[[ 3 ]], D = abcd[[ 4 ]] },
          With[ { abLines = #[[ 1, 1 ]] & /@ FindLine[ graph, A, B, All ],
                  cdLines = #[[ 1, 1 ]] & /@ FindLine[ graph, C, D, All ] },
            If[ ! AnyTrue[ Tuples[ { abLines, cdLines } ], IntersectingQ @@ # & ],
              True,
              With[ { acLines = #[[ 1, 1 ]] & /@ FindLine[ graph, A, C, All ],
                      bdLines = #[[ 1, 1 ]] & /@ FindLine[ graph, B, D, All ] },
                AnyTrue[ Tuples[ { acLines, bdLines } ], IntersectingQ @@ # & ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]


(* ProjectivePlaneGraphQ: graph satisfies W1 + W2 + W3 plus a
   non-degeneracy witness (four vertices, no three collinear). *)

ProjectivePlaneGraphQ[ graph_Graph ] :=
  Module[ { verts },
    verts = VertexList[ graph ];
    Length[ verts ] >= 4 &&
    WhiteheadW1Q[ graph ] &&
    WhiteheadW2Q[ graph ] &&
    WhiteheadW3Q[ graph ] &&
    AnyTrue[ Subsets[ verts, { 4 } ],
      quad |-> ! AnyTrue[ Subsets[ quad, { 3 } ], CollinearQ[ graph, # ] & ]
    ]
  ]
