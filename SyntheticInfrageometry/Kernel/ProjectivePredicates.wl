Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Pointwise predicates ===================== *)

(* SameDirectionQ[g, O, v, w]: v and w lie in the same pencil-direction
   at O - some maximal geodesic through O contains both. *)

SameDirectionQ[ graph_Graph, O_, v_, w_ ] :=
  v === w || AnyTrue[ FindLine[ graph, O, v, All ][ "Realisations" ], MemberQ[ #, w ] & ]


(* CollinearQ: there exists a single canonical line containing every
   listed vertex (the synthetic-incidence "lie on a common line"). *)

CollinearQ[ graph_Graph, verts_List ] :=
  Length[ DeleteDuplicates @ verts ] <= 1 ||
    FindCommonLine[ graph, verts, UpTo[ 1 ] ][ "Length" ] > 0


(* ConcurrentQ: a set of lines shares a common vertex - the dual of
   collinearity. *)

ConcurrentQ[ graph_Graph, lines_List ] :=
  Length[ lines ] <= 1 ||
    FindCommonPoint[ graph, lines, UpTo[ 1 ] ][ "Length" ] > 0


(* UniquePencilQ[g, O]: every direction at O is single-valued - there is
   exactly one maximal geodesic through O ending at v, for every v != O.
   The pointwise-at-O version of the geodetic property. *)

UniquePencilQ[ graph_Graph, O_ ] :=
  AllTrue[ DeleteCases[ VertexList[ graph ], O ],
    FindLine[ graph, O, #, All ][ "Length" ] == 1 & ]


(* UniqueCollinearQ: exactly one canonical line contains every listed
   vertex; the unique-witness companion of CollinearQ. *)

UniqueCollinearQ[ graph_Graph, verts_List ] :=
  FindCommonLine[ graph, verts, All ][ "Length" ] == 1


(* UniqueConcurrentQ: the listed lines share exactly one common vertex;
   the unique-witness companion of ConcurrentQ. *)

UniqueConcurrentQ[ graph_Graph, lines_List ] :=
  FindCommonPoint[ graph, lines, All ][ "Length" ] == 1


(* ===================== Whitehead axioms ===================== *)

(* The three Whitehead incidence axioms for projective planes restated on
   graphs.  W1: every line has at least three points.  W2: through any
   two distinct vertices passes exactly one line (geodetic-graph
   condition).  W3: if some line through {A, B} meets some line through
   {C, D}, then some line through {A, C} meets some line through {B, D}
   (Pasch / inner-Pappus form).  ProjectivePlaneGraphQ adds a
   non-degeneracy clause (some four vertices in general position). *)

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
          With[ { abLines = FindLine[ graph, A, B, All ][ "Realisations" ],
                  cdLines = FindLine[ graph, C, D, All ][ "Realisations" ] },
            If[ ! AnyTrue[ Tuples[ { abLines, cdLines } ], IntersectingQ @@ # & ],
              True,
              With[ { acLines = FindLine[ graph, A, C, All ][ "Realisations" ],
                      bdLines = FindLine[ graph, B, D, All ][ "Realisations" ] },
                AnyTrue[ Tuples[ { acLines, bdLines } ], IntersectingQ @@ # & ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]

(* ProjectivePlaneGraphQ: graph satisfies W1 + W2 + W3 plus a
   non-degeneracy witness (four vertices, no three collinear) - the
   synthetic projective-plane conditions. *)

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
