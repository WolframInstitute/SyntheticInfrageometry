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

(* ===== InfraCircle with FindCircle ===== *)

VerificationTest[
  With[{
    scene = InfraScene[{p, c}, {
      p == InfraPoint[1],
      c == InfraCircle[p, 2]
    }],
    g = PetersenGraph[]
  },
    With[{instances = FindInfraScene[scene, g]},
      Length[instances] >= 1 &&
      AllTrue[instances, inst |-> ListQ[inst[[1]][c]] && Length[inst[[1]][c]] >= 3]
    ]
  ],
  True,
  TestID -> "FindInfraScene-InfraCircle-FindCircle"
]

EndTestSection[]
