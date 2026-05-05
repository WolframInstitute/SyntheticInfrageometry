Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine /@ FindLine[ graph, #[[ 1 ]], #[[ 2 ]], All ][ "Realisations" ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]


(* ===================== FindPencil ===================== *)

(* The pencil at a vertex O is the set of direction-classes through O,
   each represented by a canonical maximal geodesic (line) through O.
   FindPencil returns an Association keyed by these canonical lines.  In
   the synthetic-projective layer, "lines through O" = pencil elements. *)

FindPencil[ graph_Graph, O_ ] :=
  Module[ { vertices, allLines, canonicals },
    vertices = DeleteCases[ VertexList[ graph ], O ];
    allLines = Flatten[
      FindLine[ graph, O, #, All ][ "Realisations" ] & /@ vertices, 1
    ];
    canonicals = DeleteDuplicates @ ( canonicalLine /@ allLines );
    Association[ # -> # & /@ canonicals ]
  ]


(* ===================== PencilDirections, PencilCardinality ===================== *)

(* PencilDirections lists the canonical lines through O, one per direction
   class.  PencilCardinality is its size - the synthetic substitute for
   "number of directions at O". *)

PencilDirections[ graph_Graph, O_ ] := Keys @ FindPencil[ graph, O ]

PencilCardinality[ graph_Graph, O_ ] := Length @ PencilDirections[ graph, O ]


(* ===================== LineCount ===================== *)

(* Total number of distinct canonical maximal geodesics in the graph;
   the projective-incidence "number of lines". *)

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]


(* ===================== FindCommonLine ===================== *)

(* Lines containing every vertex in the input list - i.e. canonical
   maximal geodesics through the first two listed vertices that also pass
   through every other listed vertex.  Constructive companion of
   CollinearQ. *)

FindCommonLine[ graph_Graph, verts_List, All ] :=
  Module[ { uverts, candidates },
    uverts = DeleteDuplicates @ verts;
    If[ Length[ uverts ] < 2, Return[ InfraSegment[ {} ] ] ];
    candidates = canonicalLine /@ FindLine[ graph, First @ uverts, uverts[[ 2 ]], All ][ "Realisations" ];
    InfraSegment[ DeleteDuplicates @ Select[ candidates, line |-> SubsetQ[ line, uverts ] ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, UpTo[ n_Integer ] ] :=
  With[ { result = FindCommonLine[ graph, verts, All ] },
    InfraSegment[ Take[ result[ "Realisations" ], UpTo[ n ] ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, n_Integer : 1 ] :=
  With[ { result = FindCommonLine[ graph, verts, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


(* ===================== FindCommonPoint ===================== *)

(* Vertices common to every listed line - the intersection of the lines.
   Constructive companion of ConcurrentQ.  Each input line is either a bare
   vertex sequence or an InfraSegment[{seq, ...}] wrapper; the wrapped form
   contributes the union of its realisations to the common-point search.   *)

FindCommonPoint[ graph_Graph, lines_List, All ] :=
  If[ Length[ lines ] == 0,
    InfraPoint[ {} ],
    InfraPoint[ Apply[ Intersection, lines /. InfraSegment[ reps_List ] :> Union @@ reps ] ]
  ]

FindCommonPoint[ graph_Graph, lines_List, UpTo[ n_Integer ] ] :=
  With[ { result = FindCommonPoint[ graph, lines, All ] },
    InfraPoint[ Take[ result[ "Realisations" ], UpTo[ n ] ] ]
  ]

FindCommonPoint[ graph_Graph, lines_List, n_Integer : 1 ] :=
  With[ { result = FindCommonPoint[ graph, lines, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]
