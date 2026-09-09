BeginTestSection["InfraScene"]

geodesicGraph      = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
geodesicCycleGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicCycleGraph;
infraSpread        = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;

(* ===== Scene Construction ===== *)

VerificationTest[
  Head @ InfraScene[{p, q}, {p == InfraPoint[], q == InfraPoint[]}],
  InfraScene,
  TestID -> "InfraScene-construction"
]

(* ===== FindInfraScene ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    MatchQ[FindInfraScene[scene, g], {__InfraInstance}]
  ],
  True,
  TestID -> "FindInfraScene-returns-list-of-instances"
]

VerificationTest[
  With[{
    scene = InfraScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    Length[FindInfraScene[scene, g]] >= 1
  ],
  True,
  TestID -> "FindInfraScene-nonempty"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[],
      q == InfraPoint[],
      s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindInfraScene[scene, g], MatchQ[InfraInstance[_Association]]]
  ],
  True,
  TestID -> "FindInfraScene-instances-wrap-associations"
]

VerificationTest[
  With[{
    scene = InfraScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    Length[FindInfraScene[scene, g]] == 5
  ],
  True,
  TestID -> "FindInfraScene-no-pruning-all-branches"
]

VerificationTest[
  With[{
    scene = InfraScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    Length[FindInfraScene[scene, g, "PruneProbability" -> 0.9]] >= 1
  ],
  True,
  TestID -> "FindInfraScene-pruning-at-least-one-survives"
]

(* ===== Fixed Vertex ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p}, {p == InfraPoint[3]}],
    g = PathGraph[Range[5]]
  },
    With[{instances = FindInfraScene[scene, g]},
      Length[instances] == 1 && instances[[1]][[1]][p] == 3
    ]
  ],
  True,
  TestID -> "FindInfraScene-fixed-vertex"
]

(* ===== InfraDistance Assertion ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[],
      q == InfraPoint[],
      s == InfraSegment[p, q],
      InfraDistance[p, q] >= 3
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindInfraScene[scene, g],
      inst |-> GraphDistance[g, inst[[1]][p], inst[[1]][q]] >= 3]
  ],
  True,
  TestID -> "FindInfraScene-InfraDistance-assertion"
]

(* ===== InfraSegmentQ Assertion ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[],
      q == InfraPoint[],
      s == InfraSegment[p, q],
      InfraSegmentQ[s]
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindInfraScene[scene, g],
      inst |-> InfraSegmentQ[g, inst[[1]][s]]]
  ],
  True,
  TestID -> "FindInfraScene-InfraSegmentQ-assertion"
]

(* ===== InfraShell with FindInfraShell ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, c}, {
      p == InfraPoint[1],
      c == InfraShell[p, 2]
    }],
    g = PetersenGraph[]
  },
    With[{instances = FindInfraScene[scene, g]},
      Length[instances] >= 1 &&
      AllTrue[instances, inst |-> ListQ[inst[[1]][c]] && Length[inst[[1]][c]] >= 3]
    ]
  ],
  True,
  TestID -> "FindInfraScene-InfraShell-FindInfraShell"
]

(* ===== InfraPlane with FindInfraBisectingHyperplane ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{a, b, h}, {
      a == InfraPoint[1],
      b == InfraPoint[5],
      h == InfraPlane[a, b]
    }],
    g = PathGraph[Range[5]]
  },
    With[{instances = FindInfraScene[scene, g]},
      Length[instances] >= 1 &&
      AllTrue[instances, inst |-> ListQ[inst[[1]][h]] && MemberQ[inst[[1]][h], 3]]
    ]
  ],
  True,
  TestID -> "FindInfraScene-InfraPlane-FindInfraBisectingHyperplane"
]

(* InfraPlane[p1, p2, {lo, hi}] threads the window through to FindInfraBisectingHyperplane.
   On PathGraph[6], 1 to 6 has odd distance; the strict {0, 0} bisector is empty
   so the no-window form yields no instances, while {-1, 1} recovers {3} and {4}. *)
VerificationTest[
  With[{
    scene = InfraScene[{a, b, h}, {
      a == InfraPoint[1],
      b == InfraPoint[6],
      h == InfraPlane[a, b, {-1, 1}]
    }],
    g = PathGraph[Range[6]]
  },
    Sort @ DeleteDuplicates[#[[1]][h] & /@ FindInfraScene[scene, g]]
  ],
  {{3}, {4}},
  TestID -> "FindInfraScene-InfraPlane-window"
]

(* ===== InfraCircle with FindInfraCircle ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, c}, {
      p == InfraPoint[6],
      c == InfraCircle[p, {1, 2}]
    }],
    g = GridGraph[{4, 4}]
  },
    With[{instances = FindInfraScene[scene, g]},
      Length[instances] >= 1 &&
      AllTrue[instances, inst |-> ListQ[inst[[1]][c]] && Length[inst[[1]][c]] >= 3]
    ]
  ],
  True,
  TestID -> "FindInfraScene-InfraCircle-FindInfraCircle"
]

(* ===== InfraGeometricStep ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{a, b, s}, {
      InfraGeometricStep[{a == InfraPoint[], b == InfraPoint[]}, "pick points"],
      InfraGeometricStep[{s == InfraSegment[a, b]}, "draw segment"]
    }],
    g = PathGraph[Range[5]]
  },
    scene["ManualSteps"] === True &&
    scene["Steps"] === {{a, b}, {s}} &&
    scene["Labels"] === {"pick points", "draw segment"}
  ],
  True,
  TestID -> "InfraGeometricStep-scene-construction"
]

VerificationTest[
  With[{
    scene = InfraScene[{a, b, s}, {
      InfraGeometricStep[{a == InfraPoint[], b == InfraPoint[]}],
      InfraGeometricStep[{s == InfraSegment[a, b]}]
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindInfraScene[scene, g], MatchQ[InfraInstance[_Association]]]
  ],
  True,
  TestID -> "InfraGeometricStep-FindInfraScene"
]

VerificationTest[
  With[{
    sceneManual = InfraScene[{p, q, s}, {
      InfraGeometricStep[{p == InfraPoint[], q == InfraPoint[]}],
      InfraGeometricStep[{s == InfraSegment[p, q]}]
    }],
    sceneAuto = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    Length[FindInfraScene[sceneManual, g]] == Length[FindInfraScene[sceneAuto, g]]
  ],
  True,
  TestID -> "InfraGeometricStep-same-results-as-auto"
]

VerificationTest[
  With[{
    scene = InfraScene[{a, b, s}, {
      InfraGeometricStep[{a == InfraPoint[], b == InfraPoint[]}],
      InfraGeometricStep[{s == InfraSegment[a, b]}],
      InfraDistance[a, b] >= 3
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindInfraScene[scene, g],
      inst |-> GraphDistance[g, inst[[1]][a], inst[[1]][b]] >= 3]
  ],
  True,
  TestID -> "InfraGeometricStep-global-assertion"
]

(* ===== Initial Bindings ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindInfraScene[scene, g, <|p -> 1|>],
      inst |-> inst[[1]][p] == 1]
  ],
  True,
  TestID -> "FindInfraScene-initial-bindings-fix-point"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    Length[FindInfraScene[scene, g, <|p -> 1, q -> 5|>]] <
    Length[FindInfraScene[scene, g]]
  ],
  True,
  TestID -> "FindInfraScene-initial-bindings-reduce-branches"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    With[{instances = FindInfraScene[scene, g, <|p -> 1, q -> 5|>]},
      Length[instances] >= 1 &&
      AllTrue[instances, inst |-> inst[[1]][p] == 1 && inst[[1]][q] == 5]
    ]
  ],
  True,
  TestID -> "FindInfraScene-initial-bindings-both-fixed"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    With[{
      step1 = FindInfraScene[scene, g, 1],
      fixed = FindInfraScene[scene, g, 1][[1, 1]]
    },
      With[{step2 = FindInfraScene[scene, g, 2, fixed]},
        AllTrue[step2,
          inst |-> inst[[1]][p] == fixed[p] && inst[[1]][q] == fixed[q]]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraScene-fix-and-advance"
]


(* ===== InfraInstance accessor ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    With[{inst = First @ FindInfraScene[scene, g]},
      InfraInstance[inst, p] === inst[[1]][p]
    ]
  ],
  True,
  TestID -> "InfraInstance-accessor-wrapped-single-symbol"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    With[{inst = First @ FindInfraScene[scene, g]},
      InfraInstance[inst, {p, q, s}] === {inst[[1]][p], inst[[1]][q], inst[[1]][s]}
    ]
  ],
  True,
  TestID -> "InfraInstance-accessor-wrapped-symbol-list"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    With[{inst = First @ FindInfraScene[scene, g]},
      InfraInstance[inst[[1]], p] === inst[[1]][p]
    ]
  ],
  True,
  TestID -> "InfraInstance-accessor-bare-association-single-symbol"
]

VerificationTest[
  With[{
    scene = InfraScene[{p, q, s}, {
      p == InfraPoint[], q == InfraPoint[], s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    With[{inst = First @ FindInfraScene[scene, g]},
      InfraInstance[inst[[1]], {p, q, s}] === {inst[[1]][p], inst[[1]][q], inst[[1]][s]}
    ]
  ],
  True,
  TestID -> "InfraInstance-accessor-bare-association-symbol-list"
]

(* ===== InfraDistance top-level form ===== *)

(* Bare vertex pair: behaves like GraphDistance. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], 1, 9],
  4,
  TestID -> "InfraDistance-bare-bare"
]

(* Bare vertex paired with a singleton multiset. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], <| 1 -> 1 |>, 9],
  4,
  TestID -> "InfraDistance-bare-multiset-singleton"
]

(* Two multi-vertex InfraPoints: default aggregation is Min over the
   cross-product of realisations.  d(1,9)=4, d(1,7)=2, d(3,9)=2, d(3,7)=4
   -> Min = 2. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], <| 1 -> 1, 3 -> 1 |>, <| 7 -> 1, 9 -> 1 |>],
  2,
  TestID -> "InfraDistance-InfraPoint-Min-default"
]

(* Same arguments under "Aggregation" -> Max gives the diameter, 4. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], <| 1 -> 1, 3 -> 1 |>, <| 7 -> 1, 9 -> 1 |>,
    "Aggregation" -> Max],
  4,
  TestID -> "InfraDistance-InfraPoint-Max"
]

(* Mean over the four pair distances = 3. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], <| 1 -> 1, 3 -> 1 |>, <| 7 -> 1, 9 -> 1 |>,
    "Aggregation" -> Mean],
  3,
  TestID -> "InfraDistance-InfraPoint-Mean"
]

(* a walk graph's vertex set is what the distance is taken over.  Segment
   {1,2,3} to vertex 9 in GridGraph[{3,3}]: min over {d(1,9), d(2,9), d(3,9)}
   = min(4, 3, 2) = 2. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], geodesicGraph @ {1, 2, 3}, 9],
  2,
  TestID -> "InfraDistance-walk-graph"
]

(* a set: the distance from vertex 1 is the min over its vertices. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], {2, 4, 6, 8}, 1],
  1,
  TestID -> "InfraDistance-set"
]

(* FindInfraPoint returns bare vertices; InfraDistance accepts one directly,
   so callers never index into a wrapper. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}], fp = First @ FindInfraPoint[GridGraph[{3, 3}], 1]},
    InfraDistance[g, fp, 9] === GraphDistance[g, fp, 9]
  ],
  True,
  TestID -> "InfraDistance-FindInfraPoint-no-extraction"
]

(* a polyline is its List of legs.  On PathGraph[Range[7]] the polyline
   1-2-3 / 3-4-5 has vertex set {1..5}; nearest reach from vertex 7 is via 5,
   distance 2. *)
VerificationTest[
  InfraDistance[ PathGraph @ Range @ 7, geodesicGraph /@ { { 1, 2, 3 }, { 3, 4, 5 } }, 7 ],
  2,
  TestID -> "InfraDistance-polyline-legs"
]

(* a cycle graph reads through its closed walk.  On CycleGraph[6] the closed
   walk {2,3,4,2} has vertex set {2,3,4}; nearest distance to vertex 1 is
   d(1,2) = 1. *)
VerificationTest[
  InfraDistance[ CycleGraph[ 6 ], geodesicCycleGraph @ { 2, 3, 4, 2 }, 1 ],
  1,
  TestID -> "InfraDistance-cycle-graph"
]

(* a set against a density.  On PathGraph[Range[5]] the set {2,3,4} is at
   distance 1, 2, 3 from vertex 1; Min = 1. *)
VerificationTest[
  InfraDistance[ PathGraph @ Range @ 5, { 2, 3, 4 }, <| 1 -> 1 |> ],
  1,
  TestID -> "InfraDistance-set-against-a-density"
]

(* two multisets.  On PathGraph[Range[5]]
   the pair ({2,3}, {4,5}) has pairwise distances (2, 3, 1, 2); Min = 1. *)
VerificationTest[
  InfraDistance[ PathGraph @ Range @ 5,
    <| 2 -> 1, 3 -> 1 |>,
    <| 4 -> 1, 5 -> 1 |> ],
  1,
  TestID -> "InfraDistance-multiset-multiset"
]

(* Symmetry: InfraDistance[g, p, q] == InfraDistance[g, q, p] for any two
   multi-realisation arguments under any aggregator over the pairwise matrix. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], p = <| 1 -> 1, 3 -> 1 |>, q = <| 7 -> 1, 9 -> 1 |> },
    And @@ Map[
      agg |-> InfraDistance[ g, p, q, "Aggregation" -> agg ] ===
              InfraDistance[ g, q, p, "Aggregation" -> agg ],
      { Min, Max, Mean } ]
  ],
  True,
  TestID -> "InfraDistance-symmetry-Min-Max-Mean"
]


(* ===== InfraIntersection / InfraUnion (standalone) ===== *)

(* the operators are graph-first, like every other public function here, and return
   the sorted vertex List on every shape *)

VerificationTest[
  InfraIntersection[ CompleteGraph @ 7,
    geodesicGraph @ { 1, 2, 3, 4 },
    geodesicGraph @ { 1, 5, 6, 3, 7 } ],
  { 1, 3 },
  TestID -> "InfraIntersection-two-walks-vertex-set"
]

VerificationTest[
  InfraIntersection[ CompleteGraph @ 6,
    geodesicGraph /@ { { 1, 2, 3 }, { 1, 4, 3 } },
    geodesicGraph @ { 3, 5, 6 } ],
  { 3 },
  TestID -> "InfraIntersection-bundle-union-then-intersect"
]

VerificationTest[
  InfraIntersection[ CompleteGraph @ 5,
    <| 1 -> 1, 2 -> 1, 3 -> 1 |>,
    geodesicGraph @ { 2, 3, 4 },
    { 3, 4, 5 } ],
  { 3 },
  TestID -> "InfraIntersection-variadic-mixed-shapes"
]

VerificationTest[
  InfraUnion[ CompleteGraph @ 4,
    <| 1 -> 1, 2 -> 1 |>,
    geodesicGraph @ { 3, 4 } ],
  { 1, 2, 3, 4 },
  TestID -> "InfraUnion-mixed-shapes"
]

(* Symbolic args stay inert so InfraScene hypotheses are not perturbed. *)
VerificationTest[
  Head @ InfraIntersection[ s1, s2 ],
  InfraIntersection,
  TestID -> "InfraIntersection-symbolic-args-inert"
]

(* A scene constructor is not a realisation: InfraCircle[c, r] names a circle
   whose vertex set exists only after dispatch, so folding it here would
   answer with the empty set before the graph is known. *)
VerificationTest[
  Head @ InfraIntersection[ InfraCircle[ ctr1, 2 ], InfraCircle[ ctr2, 2 ] ],
  InfraIntersection,
  TestID -> "InfraIntersection-scene-constructor-args-inert"
]


(* ===== Euclid I.1 ===== *)

(* The equilateral-triangle construction: the apexes are the vertices lying on
   both circles of radius d(a, b) centred at a and at b.  On PetersenGraph[]
   with a = 1, b = 7 (d = 2) that intersection is {5, 9, 10}. *)
VerificationTest[
  With[ { g = PetersenGraph[ ],
          scene = InfraScene[ { ea, eb, ec }, {
            ec == InfraIntersection[
              InfraCircle[ ea, InfraDistance[ ea, eb ] ],
              InfraCircle[ eb, InfraDistance[ ea, eb ] ] ] } ] },
    Sort @ DeleteDuplicates[
      #[[ 1 ]][ ec ] & /@ Quiet[ FindInfraScene[ scene, g, <| ea -> 1, eb -> 7 |> ], FindInfraCircle::uncertified ] ]
  ],
  { 5, 9, 10 },
  TestID -> "FindInfraScene-EuclidI1-apexes"
]

(* Each apex is equidistant from both foci, at exactly the base length. *)
VerificationTest[
  With[ { g = PetersenGraph[ ],
          scene = InfraScene[ { ea, eb, ec }, {
            ec == InfraIntersection[
              InfraCircle[ ea, InfraDistance[ ea, eb ] ],
              InfraCircle[ eb, InfraDistance[ ea, eb ] ] ] } ] },
    With[ { apexes = #[[ 1 ]][ ec ] & /@ Quiet[ FindInfraScene[ scene, g, <| ea -> 1, eb -> 7 |> ], FindInfraCircle::uncertified ] },
      apexes =!= { } &&
      AllTrue[ apexes,
        v |-> GraphDistance[ g, 1, v ] == GraphDistance[ g, 7, v ] == GraphDistance[ g, 1, 7 ] ]
    ]
  ],
  True,
  TestID -> "FindInfraScene-EuclidI1-equilateral"
]

(* The scene agrees with the intersection taken by hand from FindInfraCircle. *)
VerificationTest[
  With[ { g = PetersenGraph[ ],
          scene = InfraScene[ { ea, eb, ec }, {
            ec == InfraIntersection[
              InfraCircle[ ea, InfraDistance[ ea, eb ] ],
              InfraCircle[ eb, InfraDistance[ ea, eb ] ] ] } ] },
    Sort @ DeleteDuplicates[
      #[[ 1 ]][ ec ] & /@ Quiet[ FindInfraScene[ scene, g, <| ea -> 1, eb -> 7 |> ], FindInfraCircle::uncertified ] ] ===
    Sort @ Quiet @ Intersection[
      Union @@ infraSpread @ FindInfraCircle[ g, 1, 2, All ],
      Union @@ infraSpread @ FindInfraCircle[ g, 7, 2, All ] ]
  ],
  True,
  TestID -> "FindInfraScene-EuclidI1-agrees-with-FindInfraCircle"
]


(* ===== Undecidable assertions ===== *)

(* An Infra*Q head the scene cannot inject the graph into would reject every
   branch silently; the scene refuses to build instead. *)
VerificationTest[
  InfraScene[ { ua, uc }, { InfraPointQ[ ua ], uc == InfraPoint[ ] } ],
  $Failed,
  { InfraScene::badassertion },
  TestID -> "InfraScene-unknown-assertion-head-refused"
]

(* A real predicate outside the scene table is refused on the same grounds. *)
VerificationTest[
  InfraScene[ { ua, us }, {
    InfraGeometricStep[ { ua == InfraPoint[ ] } ], InfraGeodesicQ[ us ] } ],
  $Failed,
  { InfraScene::badassertion },
  TestID -> "InfraScene-unknown-assertion-head-refused-manual-steps"
]

(* Heads in the table are untouched by the guard. *)
VerificationTest[
  Head @ InfraScene[ { ka, kb, ks }, {
    ka == InfraPoint[ ], kb == InfraPoint[ ], ks == InfraSegment[ ka, kb ],
    InfraSegmentQ[ ks ], InfraDistance[ ka, kb ] >= 3 } ],
  InfraScene,
  TestID -> "InfraScene-known-assertion-heads-accepted"
]

(* A known head at an arity the table has no rule for misses its rewrite and
   stays inert, exactly like an unknown head -- so it is refused the same way. *)
VerificationTest[
  InfraScene[ { ya, yb, ys }, {
    ys == InfraSegment[ ya, yb ], InfraSegmentQ[ ys, 2 ] } ],
  $Failed,
  { InfraScene::badassertion },
  TestID -> "InfraScene-known-head-wrong-arity-refused"
]


(* ===== Structural invariants ===== *)

(* An exported symbol with no definitions of any kind can only be a scene token:
   an assertion head, a construction constructor, or the step container.  The
   symbols below are exactly those: since T5 every construction head is one, the
   payload rules having gone with the payloads.  One more means a symbol was
   exported with a usage message and no meaning, which is how InfraPlaneQ hid. *)
VerificationTest[
  Select[ Names[ "WolframInstitute`SyntheticInfrageometry`*" ],
    n |-> AllTrue[
      { DownValues, UpValues, SubValues, OwnValues, FormatValues, NValues },
      f |-> ReleaseHold @ Map[ f, ToExpression[ n, InputForm, Hold ] ] === { } ] ],
  { "InfraBall", "InfraCircle", "InfraEllipse", "InfraEllipticShell", "InfraGeometricStep",
    "InfraIntersectQ", "InfraLine", "InfraPlane", "InfraPoint", "InfraPolygon", "InfraPolyline",
    "InfraRay", "InfraRevolution", "InfraSegment", "InfraShell", "InfraTriangle", "InfraWalk" },
  TestID -> "InfraScene-valueless-exports-are-scene-tokens"
]

(* And each is a live token, not a leftover: the assertion head is
   accepted by the guard, the constructor is dispatched into vertex sets, and the
   container carries a manual step. *)
VerificationTest[
  Head @ InfraScene[ { ta, tb }, { InfraIntersectQ[ ta, tb ] } ],
  InfraScene,
  TestID -> "InfraScene-token-InfraIntersectQ-is-an-assertion-head"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    And @@ ( inst |-> With[ { vs = inst[[ 1 ]][ tr ] },
        SubsetQ[ vs, { 1, 2, 3, 4 } ] && SubsetQ[ VertexList @ g, vs ] ] ) /@
      FindInfraScene[
        InfraScene[ { tr }, { tr == InfraRevolution[ { 1, 2, 3, 4 }, 1 ] } ], g ] ],
  True,
  TestID -> "InfraScene-token-InfraRevolution-is-a-constructor"
]

VerificationTest[
  With[ { scene = InfraScene[ { sa, sb }, {
      InfraGeometricStep[ { sa == InfraPoint[ ] } ],
      InfraGeometricStep[ { sb == InfraPoint[ ] } ] } ] },
    scene[ "ManualSteps" ] === True && scene[ "Steps" ] === { { sa }, { sb } } ],
  True,
  TestID -> "InfraScene-token-InfraGeometricStep-carries-a-step"
]

(* Every scene assertion delegates to a named predicate.  InfraPlaneQ was the
   one entry whose semantics lived inlined in the rule table instead of behind a
   name, which is why InfraPlane was the only level set with no *Q at all. *)
VerificationTest[
  With[ { heldRHS = Cases[
      WolframInstitute`SyntheticInfrageometry`PackageScope`sceneAssertionRules[ Null ],
      RuleDelayed[ _, rhs_ ] :> Hold[ rhs ] ] },
    { Select[ heldRHS, ! MatchQ[ #, Hold[ _Symbol[ ___ ] ] ] & ],
      Select[ Extract[ #, { 1, 0 } ] & /@ heldRHS,
        Context[ # ] =!= "System`" &&
          ! MemberQ[ Names[ "WolframInstitute`SyntheticInfrageometry`*" ],
            SymbolName[ # ] ] & ] } ],
  { { }, { } },
  TestID -> "InfraScene-assertion-rules-delegate-to-named-predicates"
]


(* ===== The scene reads vertex sets by shape, not by AtomQ ===== *)

(* InfraIntersection inside a scene resolves each operand to its vertex set through
   the anchor rule.  On a list-labelled substrate the wrapper-era reading -- a bare
   vertex is AtomQ -- split every vertex into its coordinates *)
VerificationTest[
  With[{g = TessellationGraph[{4, 4}, 2]},
    {c = First @ VertexList @ g},
    Sort @ InfraIntersection[ g, FindInfraBall[g, c, 1], FindInfraBall[g, c, 2] ] ===
      Sort @ FindInfraBall[g, c, 1]],
  True,
  TestID -> "InfraIntersection-on-a-list-labelled-substrate"
]

(* the same through the scene engine: the intersection of two balls about one
   centre is the smaller ball, and every binding is a substrate vertex *)
VerificationTest[
  With[{g = TessellationGraph[{4, 4}, 2]},
    {c = First @ VertexList @ g},
    {scene = InfraScene[{p}, {p == InfraIntersection[InfraBall[c, 1], InfraBall[c, 2]]}]},
    {instances = FindInfraScene[scene, g]},
    AllTrue[instances, VertexQ[g, InfraInstance[#, p]] &] &&
      Sort[InfraInstance[#, p] & /@ instances] === Sort @ FindInfraBall[g, c, 1]],
  True,
  TestID -> "InfraScene-intersection-binds-substrate-vertices"
]


EndTestSection[]
