Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Pointwise predicates ===================== *)

SameDirectionQ[ graph_Graph, O_, v_, w_ ] :=
  v === w || AnyTrue[ FindLine[ graph, O, v, All ], MemberQ[ #, w ] & ]

CollinearQ[ graph_Graph, verts_List ] :=
  Length[ DeleteDuplicates @ verts ] <= 1 ||
    Length[ FindCommonLine[ graph, verts, UpTo[ 1 ] ] ] > 0

ConcurrentQ[ graph_Graph, lines_List ] :=
  Length[ lines ] <= 1 ||
    Length[ FindCommonPoint[ graph, lines, UpTo[ 1 ] ] ] > 0

UniquePencilQ[ graph_Graph, O_ ] :=
  AllTrue[ DeleteCases[ VertexList[ graph ], O ],
    Length[ FindLine[ graph, O, #, All ] ] == 1 & ]

UniqueCollinearQ[ graph_Graph, verts_List ] :=
  Length[ FindCommonLine[ graph, verts, All ] ] == 1


(* ===================== Whitehead axioms ===================== *)

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
          With[ { abLines = FindLine[ graph, A, B, All ],
                  cdLines = FindLine[ graph, C, D, All ] },
            If[ ! AnyTrue[ Tuples[ { abLines, cdLines } ], IntersectingQ @@ # & ],
              True,
              With[ { acLines = FindLine[ graph, A, C, All ],
                      bdLines = FindLine[ graph, B, D, All ] },
                AnyTrue[ Tuples[ { acLines, bdLines } ], IntersectingQ @@ # & ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]

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
