BeginTestSection["InfraScene"]

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
      inst |-> SegmentQ[g, inst[[1]][s]]]
  ],
  True,
  TestID -> "FindInfraScene-InfraSegmentQ-assertion"
]

(* ===== InfraShell with FindShell ===== *)

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
  TestID -> "FindInfraScene-InfraShell-FindShell"
]

(* ===== InfraPlane with FindBisectingHyperplane ===== *)

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
  TestID -> "FindInfraScene-InfraPlane-FindBisectingHyperplane"
]

(* InfraPlane[p1, p2, {lo, hi}] threads the window through to FindBisectingHyperplane.
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

(* ===== InfraCircle with FindCircle ===== *)

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
  TestID -> "FindInfraScene-InfraCircle-FindCircle"
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

(* Bare vertex paired with an InfraPoint singleton wrapper. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], InfraPoint[{1}], 9],
  4,
  TestID -> "InfraDistance-bare-InfraPoint-singleton"
]

(* Two multi-vertex InfraPoints: default aggregation is Min over the
   cross-product of realisations.  d(1,9)=4, d(1,7)=2, d(3,9)=2, d(3,7)=4
   -> Min = 2. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], InfraPoint[{1, 3}], InfraPoint[{7, 9}]],
  2,
  TestID -> "InfraDistance-InfraPoint-Min-default"
]

(* Same arguments under "Aggregation" -> Max gives the diameter, 4. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], InfraPoint[{1, 3}], InfraPoint[{7, 9}],
    "Aggregation" -> Max],
  4,
  TestID -> "InfraDistance-InfraPoint-Max"
]

(* Mean over the four pair distances = 3. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], InfraPoint[{1, 3}], InfraPoint[{7, 9}],
    "Aggregation" -> Mean],
  3,
  TestID -> "InfraDistance-InfraPoint-Mean"
]

(* InfraSegment realisations are paths; Union flattens them to a vertex set.
   Segment {1,2,3} to vertex 9 in GridGraph[{3,3}]: min over {d(1,9),
   d(2,9), d(3,9)} = min(4, 3, 2) = 2. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], InfraSegment[{{1, 2, 3}}], 9],
  2,
  TestID -> "InfraDistance-InfraSegment-bare"
]

(* InfraShell with two set-realisations (different radius shells around 5):
   distance from vertex 1 = min over the union of those sets. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}], InfraShell[{{2, 4, 6, 8}}], 1],
  1,
  TestID -> "InfraDistance-InfraShell-bare"
]

(* InfraPencil recursion: the rays' realisations are pulled in via
   infraVertexSet's Map. *)
VerificationTest[
  InfraDistance[GridGraph[{3, 3}],
    InfraPencil[{InfraRay[{{5, 2}}], InfraRay[{{5, 4}}]}], 1],
  1,
  TestID -> "InfraDistance-InfraPencil-bare"
]

(* FindPoint returns InfraPoint[{v}]; InfraDistance accepts it directly,
   so callers no longer need First @ FindPoint[g, 1]["Realizations"]. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}], fp = FindPoint[GridGraph[{3, 3}], 1]},
    InfraDistance[g, fp, 9] === GraphDistance[g, First @ fp["Realizations"], 9]
  ],
  True,
  TestID -> "InfraDistance-FindPoint-no-extraction"
]


EndTestSection[]
