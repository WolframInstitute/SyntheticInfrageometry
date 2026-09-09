Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Pointwise predicates ===================== *)

(* v, w lie in the same direction at O: some ray from O through v contains w.  On lines this was CollinearQ[graph, {O, v, w}] under another name, and it called the two sides of O one direction *)

SameDirectionQ[ graph_Graph, O_, v_, w_ ] :=
  v === w || AnyTrue[ infraSpread @ FindInfraRay[ graph, O, v, All ], MemberQ[ #, w ] & ]


(* some canonical line contains every listed vertex *)

CollinearQ[ graph_Graph, verts_List ] :=
  Length[ DeleteDuplicates @ verts ] <= 1 ||
    Length[ infraSpread @ FindInfraCommonLine[ graph, verts, UpTo[ 1 ] ] ] > 0


(* the listed lines share a common vertex: the dual of collinearity *)

ConcurrentQ[ graph_Graph, lines_List ] :=
  Length[ lines ] <= 1 ||
    Length[ FindInfraCommonPoint[ graph, lines, UpTo[ 1 ] ] ] > 0



(* exactly one canonical line contains every listed vertex *)

UniqueCollinearQ[ graph_Graph, verts_List ] :=
  Length[ infraSpread @ FindInfraCommonLine[ graph, verts, All ] ] == 1


(* the listed lines share exactly one common vertex *)

UniqueConcurrentQ[ graph_Graph, lines_List ] :=
  Length[ FindInfraCommonPoint[ graph, lines, All ] ] == 1


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
          { abLines = infraSpread @ FindInfraLine[ graph, A, B, All ],
            cdLines = infraSpread @ FindInfraLine[ graph, C, D, All ] },
          If[ ! AnyTrue[ Tuples[ { abLines, cdLines } ], IntersectingQ @@ # & ],
            True,
            With[ { acLines = infraSpread @ FindInfraLine[ graph, A, C, All ],
                    bdLines = infraSpread @ FindInfraLine[ graph, B, D, All ] },
              AnyTrue[ Tuples[ { acLines, bdLines } ], IntersectingQ @@ # & ]
            ]
          ]
        ]
      ]
    ]
  ]


(* W1 + W2 + W3 plus a non-degeneracy witness: four vertices, no three collinear *)

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
