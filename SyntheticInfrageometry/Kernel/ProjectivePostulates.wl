Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine /@ FindLine[ graph, #[[ 1 ]], #[[ 2 ]], All ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]


(* ===================== FindPencil ===================== *)

FindPencil[ graph_Graph, O_ ] :=
  Module[ { vertices, allLines, canonicals },
    vertices = DeleteCases[ VertexList[ graph ], O ];
    allLines = Flatten[
      FindLine[ graph, O, #, All ] & /@ vertices, 1
    ];
    canonicals = DeleteDuplicates @ ( canonicalLine /@ allLines );
    Association[ # -> # & /@ canonicals ]
  ]


(* ===================== PencilDirections, PencilCardinality ===================== *)

PencilDirections[ graph_Graph, O_ ] := Keys @ FindPencil[ graph, O ]

PencilCardinality[ graph_Graph, O_ ] := Length @ PencilDirections[ graph, O ]


(* ===================== LineCount ===================== *)

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]


(* ===================== FindCommonLine ===================== *)

FindCommonLine[ graph_Graph, verts_List, All ] :=
  Module[ { uverts, candidates },
    uverts = DeleteDuplicates @ verts;
    If[ Length[ uverts ] < 2, Return[ {} ] ];
    candidates = canonicalLine /@ FindLine[ graph, First @ uverts, uverts[[ 2 ]], All ];
    DeleteDuplicates @ Select[ candidates, line |-> SubsetQ[ line, uverts ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, UpTo[ n_Integer ] ] :=
  Take[ FindCommonLine[ graph, verts, All ], UpTo[ n ] ]

FindCommonLine[ graph_Graph, verts_List, n_Integer : 1 ] :=
  With[ { result = FindCommonLine[ graph, verts, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== FindCommonPoint ===================== *)

FindCommonPoint[ graph_Graph, lines_List, All ] :=
  If[ Length[ lines ] == 0, {}, Apply[ Intersection, lines ] ]

FindCommonPoint[ graph_Graph, lines_List, UpTo[ n_Integer ] ] :=
  Take[ FindCommonPoint[ graph, lines, All ], UpTo[ n ] ]

FindCommonPoint[ graph_Graph, lines_List, n_Integer : 1 ] :=
  With[ { result = FindCommonPoint[ graph, lines, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]
