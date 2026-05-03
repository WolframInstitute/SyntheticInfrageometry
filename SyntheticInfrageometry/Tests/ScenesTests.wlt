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
    Length[FindInfraScene[scene, g, "PruningProbability" -> 0.9]] >= 1
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

EndTestSection[]
