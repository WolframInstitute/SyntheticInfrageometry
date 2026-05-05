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


(* ===================== FindRay ===================== *)

(* A ray at base vertex origin in the direction of v is a maximal
   geodesic through origin containing v -- the same shape as FindLine's
   output, but framed projectively as "the line through origin in v's
   direction".  FindRay enumerates every such line and returns them as
   InfraRay[{ray1, ray2, ...}].  Multiple realisations belong to one
   direction class (the equivalence class of canonicalLine).            *)

findRayCore[ graph_Graph, origin_, v_ ] :=
  DeleteDuplicatesBy[ FindLine[ graph, origin, v, All ][ "Realisations" ], canonicalLine ]

FindRay[ graph_Graph, origin_, v_, All ] :=
  infraSpreadAndCartesian[ InfraRay, All, findRayCore[ graph, ##] &, origin, v ]

FindRay[ graph_Graph, origin_, v_, UpTo[ n_Integer ] ] :=
  With[ { result = FindRay[ graph, origin, v, All ] },
    If[ result === $Failed, $Failed,
      InfraRay[ Take[ result[ "Realisations" ], UpTo[ n ] ] ]
    ]
  ]

FindRay[ graph_Graph, origin_, v_, n_Integer : 1 ] :=
  With[ { result = FindRay[ graph, origin, v, UpTo[ n ] ] },
    If[ result === $Failed || result[ "Length" ] < n, $Failed, result ]
  ]


(* ===================== FindPencil ===================== *)

(* The pencil at vertex origin is the set of direction classes through
   origin, each realised by every maximal geodesic through origin sharing
   that direction.  FindPencil returns the pencil as InfraPencil of
   constituent InfraRays, one per direction class.  Pencil cardinality is
   the wrapper's ["Length"]; multi-anchor origin spreads Cartesian and
   unions the rays across choices.                                       *)

findPencilCore[ graph_Graph, origin_ ] :=
  Module[ { otherVerts, allLines, byCanonical },
    otherVerts = DeleteCases[ VertexList[ graph ], origin ];
    allLines = Flatten[
      FindLine[ graph, origin, #, All ][ "Realisations" ] & /@ otherVerts,
      1
    ];
    byCanonical = GroupBy[ allLines, canonicalLine ];
    InfraRay[ DeleteDuplicatesBy[ #, canonicalLine ] ] & /@ Values[ byCanonical ]
  ]

FindPencil[ graph_Graph, InfraPoint[ origins_List ] ] :=
  InfraPencil[ DeleteDuplicates @ Flatten[ findPencilCore[ graph, # ] & /@ origins, 1 ] ]

FindPencil[ graph_Graph, origin_ ] :=
  InfraPencil[ findPencilCore[ graph, origin ] ]


(* ===================== PencilDirections, PencilCardinality ===================== *)

(* PencilDirections lists the canonical lines through origin, one per
   direction class -- the canonical representatives of the constituent
   InfraRays.  PencilCardinality is the pencil's size.                    *)

PencilDirections[ graph_Graph, origin_ ] :=
  canonicalLine[ #[ "First" ] ] & /@ FindPencil[ graph, origin ][ "Realisations" ]

PencilCardinality[ graph_Graph, origin_ ] := FindPencil[ graph, origin ][ "Length" ]


(* ===================== LineCount ===================== *)

(* Total number of distinct canonical maximal geodesics in the graph;
   the projective-incidence "number of lines". *)

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]


(* ===================== FindCommonLine ===================== *)

(* Lines containing every vertex in the input list -- canonical maximal
   geodesics through the first two listed vertices that also pass through
   every other listed vertex.  Constructive companion of CollinearQ.
   Wrapped entries (InfraPoint, InfraSegment, InfraRay, InfraPencil)
   collapse to the union of their vertex realisations before the search. *)

findCommonLineCore[ graph_Graph, verts_List ] :=
  Module[ { uverts, candidates },
    uverts = DeleteDuplicates @ Catenate[ infraUnionSpread /@ verts ];
    If[ Length[ uverts ] < 2, Return[ {} ] ];
    candidates = canonicalLine /@ FindLine[ graph, First @ uverts, uverts[[ 2 ]], All ][ "Realisations" ];
    DeleteDuplicates @ Select[ candidates, line |-> SubsetQ[ line, uverts ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, All ] :=
  InfraSegment[ findCommonLineCore[ graph, verts ] ]

FindCommonLine[ graph_Graph, verts_List, UpTo[ n_Integer ] ] :=
  With[ { result = FindCommonLine[ graph, verts, All ] },
    InfraSegment[ Take[ result[ "Realisations" ], UpTo[ n ] ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, n_Integer : 1 ] :=
  With[ { result = FindCommonLine[ graph, verts, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


(* ===================== FindCommonPoint ===================== *)

(* Vertices common to every listed line -- the intersection of the lines.
   Constructive companion of ConcurrentQ.  Each input line is either a
   bare vertex sequence or a wrapped InfraSegment / InfraRay / InfraPencil;
   wrapped entries contribute the union of their vertex realisations.    *)

linePointSet[ InfraSegment[ reps_List ] ] := Union @@ reps
linePointSet[ InfraRay    [ reps_List ] ] := Union @@ reps
linePointSet[ InfraPencil [ rays_List ] ] := Union @@ Catenate[ #[ "Realisations" ] & /@ rays ]
linePointSet[ line_List ] := line

FindCommonPoint[ graph_Graph, lines_List, All ] :=
  If[ Length[ lines ] == 0,
    InfraPoint[ {} ],
    InfraPoint[ Apply[ Intersection, linePointSet /@ lines ] ]
  ]

FindCommonPoint[ graph_Graph, lines_List, UpTo[ n_Integer ] ] :=
  With[ { result = FindCommonPoint[ graph, lines, All ] },
    InfraPoint[ Take[ result[ "Realisations" ], UpTo[ n ] ] ]
  ]

FindCommonPoint[ graph_Graph, lines_List, n_Integer : 1 ] :=
  With[ { result = FindCommonPoint[ graph, lines, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]
