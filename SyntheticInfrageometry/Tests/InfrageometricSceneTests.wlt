BeginTestSection["InfrageometricScene"]

(* ===== Scene Construction ===== *)

VerificationTest[
  Head @ InfrageometricScene[{p, q}, {p == InfraPoint[], q == InfraPoint[]}],
  InfrageometricScene,
  TestID -> "InfrageometricScene-construction"
]

(* ===== FindSceneInstance ===== *)

VerificationTest[
  With[{
    scene = InfrageometricScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    MatchQ[FindSceneInstance[scene, g], {__InfrageometricInstance}]
  ],
  True,
  TestID -> "FindSceneInstance-returns-list-of-instances"
]

VerificationTest[
  With[{
    scene = InfrageometricScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    Length[FindSceneInstance[scene, g]] >= 1
  ],
  True,
  TestID -> "FindSceneInstance-nonempty"
]

VerificationTest[
  With[{
    scene = InfrageometricScene[{p, q, s}, {
      p == InfraPoint[],
      q == InfraPoint[],
      s == InfraSegment[p, q]
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindSceneInstance[scene, g], MatchQ[InfrageometricInstance[_Association]]]
  ],
  True,
  TestID -> "FindSceneInstance-instances-wrap-associations"
]

VerificationTest[
  With[{
    scene = InfrageometricScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    Length[FindSceneInstance[scene, g]] == 5
  ],
  True,
  TestID -> "FindSceneInstance-no-pruning-all-branches"
]

VerificationTest[
  With[{
    scene = InfrageometricScene[{p}, {p == InfraPoint[]}],
    g = PathGraph[Range[5]]
  },
    Length[FindSceneInstance[scene, g, "PruningProbability" -> 0.9]] >= 1
  ],
  True,
  TestID -> "FindSceneInstance-pruning-at-least-one-survives"
]

(* ===== Fixed Vertex ===== *)

VerificationTest[
  With[{
    scene = InfrageometricScene[{p}, {p == InfraPoint[3]}],
    g = PathGraph[Range[5]]
  },
    With[{instances = FindSceneInstance[scene, g]},
      Length[instances] == 1 && instances[[1]][[1]][p] == 3
    ]
  ],
  True,
  TestID -> "FindSceneInstance-fixed-vertex"
]

(* ===== InfraDistance Assertion ===== *)

VerificationTest[
  With[{
    scene = InfrageometricScene[{p, q, s}, {
      p == InfraPoint[],
      q == InfraPoint[],
      s == InfraSegment[p, q],
      InfraDistance[p, q] >= 3
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindSceneInstance[scene, g],
      inst |-> GraphDistance[g, inst[[1]][p], inst[[1]][q]] >= 3]
  ],
  True,
  TestID -> "FindSceneInstance-InfraDistance-assertion"
]

(* ===== InfraSegmentQ Assertion ===== *)

VerificationTest[
  With[{
    scene = InfrageometricScene[{p, q, s}, {
      p == InfraPoint[],
      q == InfraPoint[],
      s == InfraSegment[p, q],
      InfraSegmentQ[s]
    }],
    g = PathGraph[Range[5]]
  },
    AllTrue[FindSceneInstance[scene, g],
      inst |-> SegmentQ[g, inst[[1]][s]]]
  ],
  True,
  TestID -> "FindSceneInstance-InfraSegmentQ-assertion"
]

(* ===== InfraCircle with FindCircle ===== *)

VerificationTest[
  With[{
    scene = InfrageometricScene[{p, c}, {
      p == InfraPoint[1],
      c == InfraCircle[p, 2]
    }],
    g = PetersenGraph[]
  },
    With[{instances = FindSceneInstance[scene, g]},
      Length[instances] >= 1 &&
      AllTrue[instances, inst |-> ListQ[inst[[1]][c]] && Length[inst[[1]][c]] >= 3]
    ]
  ],
  True,
  TestID -> "FindSceneInstance-InfraCircle-FindCircle"
]

EndTestSection[]
