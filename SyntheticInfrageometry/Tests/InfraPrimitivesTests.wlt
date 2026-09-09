toDensity       = WolframInstitute`SyntheticInfrageometry`PackageScope`toDensity;
walkGraph       = WolframInstitute`SyntheticInfrageometry`PackageScope`walkGraph;
geodesicGraph   = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
polylineToKnots = WolframInstitute`SyntheticInfrageometry`PackageScope`polylineToKnots;

(* Nothing is wrapped: the shape is the kind.  A point is a vertex, a set a sorted
   vertex list, a density <| v -> m |>, and every 1-d object a Graph.  So the
   accessors the wrapper heads used to own -- "Realizations", "Multiplicity",
   "Start", "End", "Length", "Volume", "Knots", the column projection wrapper[[i]]
   and the auto-flatten rule -- are gone, and their work is done by Wolfram's own
   operations on those shapes: Length, EdgeCount, VertexCount, Keys, Values, Total,
   and the degree-0 vertices of a DAG. *)


(* ----- the density layer: <| atom -> weight |> ----- *)

(* a List is one Counts away from the density; repetition becomes mass *)
VerificationTest[
  { toDensity[ PathGraph @ Range[ 3 ], { 1, 1, 2 } ], Counts[ { a, a, b } ] },
  { <| 1 -> 2, 2 -> 1 |>, <| a -> 2, b -> 1 |> },
  TestID -> "list-reads-as-density-counts"
]

(* the all-ones density is still a density: nothing collapses it to its support, and
   Keys is the explicit step down to the set, which drops the masses *)
VerificationTest[
  With[ { fam = <| a -> 1, b -> 1 |> },
    { Head @ fam, Keys @ fam } ],
  { Association, { a, b } },
  TestID -> "all-ones-density-stays-a-density"
]

(* the density algebra is the Association's own: Keys, Values, Total, Length *)
VerificationTest[
  With[ { fam = <| a -> 2, b -> 1 |> },
    { Keys @ fam, Values @ fam, Total @ fam, Length @ fam } ],
  { { a, b }, { 2, 1 }, 3, 2 },
  TestID -> "density-weight-algebra-is-the-association"
]

(* the k-th element of a density is read with the Association's own Part / Keys:
   [[k]] is the k-th MASS, Keys[[k]] the k-th vertex *)
VerificationTest[
  With[ { s = <| a -> 1, b -> 1, c -> 1 |> },
    { Keys[ s ][[ 2 ]], First @ Keys @ s, Keys @ s[[ ;; 2 ]] } ],
  { b, a, { a, b } },
  TestID -> "density-Part-through-Keys"
]

(* a set is a sorted vertex list, so its size is Length and Wolfram's set algebra
   applies to it directly *)
VerificationTest[
  With[ { s = { 1, 2, 3, 4 } },
    { Length @ s, Union[ s, { 4, 5 } ], Intersection[ s, { 3, 4, 9 } ], SubsetQ[ s, { 2, 3 } ] } ],
  { 4, { 1, 2, 3, 4, 5 }, { 3, 4 }, True },
  TestID -> "set-is-a-list-and-takes-the-set-algebra"
]


(* ----- synthetic invariants are read off the primitives, not off a wrapper ----- *)

(* on a path B_r(end) = r + 1; a set gives one row per vertex *)
VerificationTest[
  { BallVolumes[ PathGraph @ Range[ 7 ], 1, { 0, 3 } ],
    BallVolumes[ PathGraph @ Range[ 7 ], { 1 }, { 0, 3 } ] },
  { { 1, 2, 3, 4 }, { { 1, 2, 3, 4 } } },
  TestID -> "point-layer-BallVolumes"
]

(* the tube of a pair thickens the metric interval, so it is never smaller than it *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    AllTrue[ TubeVolumes[ g, 13, 25, 1 ], # >= Length @ MetricInterval[ g, 13, 25 ] & ] ],
  True,
  TestID -> "point-layer-TubeVolumes"
]

(* the interval count at slack 0 counts the metric interval *)
VerificationTest[
  IntervalVolumes[ PathGraph @ Range[ 7 ], 1, 4, 0 ],
  4,
  TestID -> "point-layer-IntervalVolumes"
]

(* dimension readout projects VolumeGrowthObservables["BallDimension"] *)
VerificationTest[
  MatchQ[ VolumeGrowthObservables[ GridGraph[ { 7, 7 } ], 25 ][ "BallDimension" ], _?NumericQ ],
  True,
  TestID -> "point-layer-Dimension-numeric"
]


(* ===================== the 1-d shape carries its own measurements ===================== *)

(* the length of a walk is its edge count, of a bundle of legs the total *)
VerificationTest[
  { EdgeCount @ geodesicGraph @ { 1, 2, 3 },
    EdgeCount @ geodesicGraph @ { 1, 4, 5, 3 },
    EdgeCount @ walkGraph @ { 1, 2, 3, 2, 1 } },
  { 2, 3, 4 },
  TestID -> "walk-length-is-EdgeCount"
]

(* a cycle graph has as many edges as vertices, so its length is either count *)
VerificationTest[
  With[ { c = First @ FindInfraCycle[ CycleGraph[ 6 ], 1 ] },
    { EdgeCount @ c, VertexCount @ c } ],
  { 6, 6 },
  TestID -> "cycle-length-equals-vertex-count"
]

(* the volume of a set is its Length; of a family of sets, the Length of each *)
VerificationTest[
  { Length @ FindInfraBall[ PathGraph @ Range[ 5 ], 3, 2 ],
    Length /@ { { 1, 2, 3 }, { 4, 5 } } },
  { 5, { 3, 2 } },
  TestID -> "set-volume-is-Length"
]

(* the source and the sink of an interval DAG are its in- and out-degree-0 vertices *)
VerificationTest[
  With[ { dag = FindInfraSegment[ GridGraph[ { 5, 5 } ], 1, 25, All ] },
    { Pick[ VertexList @ dag, VertexInDegree @ dag, 0 ],
      Pick[ VertexList @ dag, VertexOutDegree @ dag, 0 ] } ],
  { { 1 }, { 25 } },
  TestID -> "DAG-source-and-sink-by-degree"
]

(* the same read on a bundle of path graphs: every realisation shares the ends *)
VerificationTest[
  With[ { reps = geodesicGraph /@ { { 1, 2, 3 }, { 1, 4, 3 } } },
    { Union @@ ( Pick[ VertexList @ #, VertexInDegree @ #, 0 ] & /@ reps ),
      Union @@ ( Pick[ VertexList @ #, VertexOutDegree @ #, 0 ] & /@ reps ) } ],
  { { 1 }, { 3 } },
  TestID -> "bundle-source-and-sink-by-degree"
]

(* a bare-vertex endpoint composes into FindInfraSegment with no unwrapping step *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], ends = { 1, 25 } },
    FindInfraSegment[ g, ends[[ 1 ]], ends[[ 2 ]], All ] === FindInfraSegment[ g, 1, 25, All ] ],
  True,
  TestID -> "FindInfraSegment-vertex-endpoints-give-DAG"
]


(* ===================== FindInfraCycle ===================== *)

VerificationTest[
  MatchQ[ FindInfraCycle[ CycleGraph[ 4 ], 1 ], { _Graph } ],
  True,
  TestID -> "FindInfraCycle-returns-cycle-graphs"
]

VerificationTest[
  Length @ FindInfraCycle[ CycleGraph[ 4 ], All ],
  1,
  TestID -> "FindInfraCycle-CycleGraph4-one-cycle"
]

VerificationTest[
  FindInfraCycle[ TreeGraph[ { 1 -> 2, 2 -> 3 } ], 1 ],
  $Failed,
  TestID -> "FindInfraCycle-tree-no-cycles"
]

VerificationTest[
  VertexCount @ First @ FindInfraCycle[ GridGraph[ { 3, 3 } ], { 4 }, 1 ],
  4,
  TestID -> "FindInfraCycle-length4-on-grid"
]

VerificationTest[
  NullHomotopicQ[ GridGraph[ { 3, 3 } ],
    First @ FindInfraCycle[ GridGraph[ { 3, 3 } ], 1 ],
    "NullHomotopicCycles" -> { 4 } ],
  True,
  TestID -> "FindInfraCycle-shortest-is-nullhomotopic-on-grid"
]


(* ===================== the polyline is its List of legs ===================== *)

(* length is the total edge count over the legs, knots the shared endpoints *)
VerificationTest[
  With[ { poly = FindInfraPolylineSubdivision[ GridGraph[ { 4, 4 } ],
            { 1, 2, 6, 5, 9, 13, 14, 15, 16 }, "MaxLength" -> 2 ] },
    { Total[ EdgeCount /@ poly ], polylineToKnots @ poly } ],
  { 8, { 1, 6, 9, 14, 16 } },
  TestID -> "polyline-length-and-knots"
]

(* the empty polyline is the empty List of legs *)
VerificationTest[
  With[ { poly = FindInfraPolylineSubdivision[ GridGraph[ { 4, 4 } ], { 1 } ] },
    { poly, Total[ EdgeCount /@ poly ], polylineToKnots @ poly } ],
  { { }, 0, { } },
  TestID -> "polyline-empty"
]


(* ===================== the guard-rail ===================== *)

(* THE test every design choice has to pass: anything a construction returns can be
   handed straight to HighlightGraph -- a vertex, a vertex list, a Graph, a List of
   Graphs, and the empty List for a class with no instance.  A wrapper never could,
   which is why there are none. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    AllTrue[
      { FindInfraPoint[ g, 2 ],
        FindInfraBall[ g, 6, 1 ],
        FindInfraShell[ g, 6, { 1, 1 } ],
        FindInfraEquidistantSet[ g, { 1, 16 } ],
        FindInfraBisectingHyperplane[ g, 1, 16 ],
        FindInfraSegment[ g, 1, 16 ],
        FindInfraSegment[ g, 1, 16, All ],
        FindInfraSegment[ g, 1, 16, UpTo[ 3 ] ],
        FindInfraLine[ g, 1, 3 ],
        FindInfraRay[ g, 1, 4 ],
        FindInfraCycle[ g, 1 ],
        FindInfraCircle[ g, 6, 1 ],
        FindInfraTriangle[ g, { 1, 4, 13 } ],
        FindInfraPolygon[ g, { 1, 4, 13 } ],
        FindInfraEllipse[ g, { 1, 16 }, 6 ],
        FindInfraPerpendicular[ g, FindInfraLine[ g, 1, 3 ], 6 ],
        FindInfraPolylineSubdivision[ g, { 1, 2, 3, 7, 11 }, "MaxLength" -> 2 ] },
      GraphQ @ HighlightGraph[ g, # ] & ] ],
  True,
  TestID -> "guard-rail-every-return-highlights"
]

(* the one exception: a density carries mass, so it is an Association and not a
   HighlightGraph argument -- Keys is the step down to the set that is *)
VerificationTest[
  With[ { m = FindInfraMidpoint[ GridGraph[ { 4, 4 } ], 1, 16 ] },
    { AssociationQ @ m, GraphQ @ HighlightGraph[ GridGraph[ { 4, 4 } ], Keys @ m ] } ],
  { True, True },
  TestID -> "guard-rail-density-is-the-exception"
]
