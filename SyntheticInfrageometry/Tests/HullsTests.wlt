BeginTestSection["Hulls"]

(* FindLineHull is a closure operator: extensive, idempotent, monotone, and
   it contains the segment hull (since I(u,v) subset L(u,v)). *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], s = { 1, 6 } },
    SubsetQ[ FindLineHull[ g, s ][ "Vertices" ], Union @ s ] ],
  True,
  TestID -> "FindLineHull-extensive"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], s = { 1, 6 } }, { h = FindLineHull[ g, s ] },
    FindLineHull[ g, h ] === h ],
  True,
  TestID -> "FindLineHull-idempotent"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], s = { 2, 8 } },
    SubsetQ[ FindLineHull[ g, s ][ "Vertices" ], FindSegmentHull[ g, s ][ "Vertices" ] ] ],
  True,
  TestID -> "segment-hull-subset-of-line-hull"
]

(* PathGraph has a single maximal line (the whole path), so the line hull of
   any two distinct vertices is the entire path -- a universal line. *)

VerificationTest[
  FindLineHull[ PathGraph @ Range[ 5 ], { 2, 4 } ],
  InfraSet[ Range[ 5 ] ],
  TestID -> "FindLineHull-path-universal"
]

VerificationTest[
  FindLineHull[ PathGraph @ Range[ 5 ], { 2, 3 } ],
  InfraSet[ Range[ 5 ] ],
  TestID -> "FindLineHull-path-universal-adjacent"
]

(* C4: the two geodesics through antipodes 1, 3 close up to the whole cycle. *)

VerificationTest[
  FindLineHull[ CycleGraph[ 4 ], { 1, 3 } ],
  InfraSet[ { 1, 2, 3, 4 } ],
  TestID -> "FindLineHull-C4-antipodal"
]

(* A singleton is its own line hull (no line meets it in >= 2 vertices). *)

VerificationTest[
  FindLineHull[ GridGraph[ { 3, 3 } ], { 5 } ],
  InfraSet[ { 5 } ],
  TestID -> "FindLineHull-singleton"
]

(* LineHullQ: the full vertex set is closed; a proper subset spanning a line is not. *)

VerificationTest[
  LineHullQ[ PathGraph @ Range[ 5 ], Range[ 5 ] ],
  True,
  TestID -> "LineHullQ-full-closed"
]

VerificationTest[
  LineHullQ[ PathGraph @ Range[ 5 ], { 2, 4 } ],
  False,
  TestID -> "LineHullQ-proper-open"
]

(* UniversalLineQ: the path is a single universal line; C4 has one (antipodes
   1, 3 span the whole cycle); C5 has none (every line covers at most 4 of 5). *)

VerificationTest[
  UniversalLineQ[ PathGraph @ Range[ 5 ] ],
  True,
  TestID -> "UniversalLineQ-path"
]

VerificationTest[
  UniversalLineQ[ CycleGraph[ 4 ], { 1, 3 } ],
  True,
  TestID -> "UniversalLineQ-C4-pair"
]

VerificationTest[
  UniversalLineQ[ CycleGraph[ 4 ] ],
  True,
  TestID -> "UniversalLineQ-C4"
]

VerificationTest[
  UniversalLineQ[ CycleGraph[ 5 ] ],
  False,
  TestID -> "UniversalLineQ-C5-none"
]

(* "LineStructure" option: closing under a fixed consistent family is a unique
   closure contained in the unrestricted (all-maximal-lines) hull.  On the
   3x3 grid the full line hull of two adjacent corners swallows the whole grid
   (universal-line collapse), but the fixed structure keeps it to one row. *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    SubsetQ[ FindLineHull[ g, { 2, 8 } ][ "Vertices" ], FindLineHull[ g, { 2, 8 }, "LineStructure" -> ls ][ "Vertices" ] ] ],
  True,
  TestID -> "FindLineHull-structure-subset-of-full"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { ls = FindLineStructure[ g ], full = FindLineHull[ g, { 1, 2 } ] },
    { via = FindLineHull[ g, { 1, 2 }, "LineStructure" -> ls ] },
    via =!= full && SubsetQ[ full[ "Vertices" ], via[ "Vertices" ] ] ],
  True,
  TestID -> "FindLineHull-structure-proper-subset"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    { via = FindLineHull[ g, { 1, 2 }, "LineStructure" -> ls ] },
    FindLineHull[ g, via, "LineStructure" -> ls ] === via ],
  True,
  TestID -> "FindLineHull-structure-idempotent"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    { via = FindLineHull[ g, { 1, 2 }, "LineStructure" -> ls ] },
    { LineHullQ[ g, via, "LineStructure" -> ls ], LineHullQ[ g, { 1, 2 }, "LineStructure" -> ls ] } ],
  { True, False },
  TestID -> "LineHullQ-structure-closed-open"
]

VerificationTest[
  FindLineHull[ CycleGraph[ 6 ], { 1, 4 }, "LineStructure" -> { { 1, 2, 3, 4 } } ],
  InfraSet[ { 1, 2, 3, 4 } ],
  TestID -> "FindLineHull-bare-line-family"
]

(* Segment side: the "LineStructure" option closes under chosen-geodesic
   stretches.  Bounded above by both the unrestricted segment hull (one chosen
   geodesic vs all geodesics) and the line hull under the same family (stretch
   vs whole line); idempotent (unique fixed point). *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    SubsetQ[ FindSegmentHull[ g, { 1, 9 } ][ "Vertices" ], FindSegmentHull[ g, { 1, 9 }, "LineStructure" -> ls ][ "Vertices" ] ] ],
  True,
  TestID -> "FindSegmentHull-structure-subset-of-full"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    SubsetQ[ FindLineHull[ g, { 1, 9 }, "LineStructure" -> ls ][ "Vertices" ],
             FindSegmentHull[ g, { 1, 9 }, "LineStructure" -> ls ][ "Vertices" ] ] ],
  True,
  TestID -> "FindSegmentHull-structure-subset-of-line-structure"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    { via = FindSegmentHull[ g, { 1, 9 }, "LineStructure" -> ls ] },
    FindSegmentHull[ g, via, "LineStructure" -> ls ] === via ],
  True,
  TestID -> "FindSegmentHull-structure-idempotent"
]

(* Ball hull: the intersection of all closed balls containing S.  A closure
   operator (extensive, idempotent), output an InfraSet, and equal by
   definition to the intersection over every center of the smallest ball at
   that center enclosing S. *)

VerificationTest[
  Head @ FindBallHull[ GridGraph[ { 6, 4 } ], { 1, 6 } ],
  InfraSet,
  TestID -> "FindBallHull-returns-InfraSet"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 4 } ], s = { 1, 6, 22 } },
    SubsetQ[ FindBallHull[ g, s ][ "Vertices" ], s ] ],
  True,
  TestID -> "FindBallHull-extensive"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 4 } ] }, { h = FindBallHull[ g, { 1, 6, 22 } ] },
    FindBallHull[ g, h ] === h ],
  True,
  TestID -> "FindBallHull-idempotent"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 4 } ] },
    BallHullQ[ g, FindBallHull[ g, { 1, 6, 22 } ] ] ],
  True,
  TestID -> "FindBallHull-ball-convex"
]

(* Defining property: w is in the hull iff d(c,w) <= max_{s} d(c,s) for every
   center c -- the intersection of the smallest enclosing balls. *)

VerificationTest[
  With[ { g = GridGraph[ { 6, 4 } ], s = { 1, 6, 22 } },
    FindBallHull[ g, s ][ "Vertices" ] ===
      Sort @ Fold[ Intersection, VertexList @ g,
        Table[ With[ { r = Max[ GraphDistance[ g, c, # ] & /@ s ] },
            Select[ VertexList @ g, GraphDistance[ g, c, # ] <= r & ] ], { c, VertexList @ g } ] ] ],
  True,
  TestID -> "FindBallHull-equals-ball-intersection"
]

(* A singleton and a closed ball are both ball-convex (B_0(v) and B_r(c)); a
   generic mid-distance pair on a path is not. *)

VerificationTest[
  FindBallHull[ GridGraph[ { 4, 4 } ], { 6 } ][ "Vertices" ],
  { 6 },
  TestID -> "FindBallHull-singleton"
]

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    BallHullQ[ g, FindInfraBall[ g, 13, 2 ] ] ],
  True,
  TestID -> "BallHullQ-ball-is-ball-convex"
]

VerificationTest[
  BallHullQ[ PathGraph @ Range[ 5 ], { 1, 5 } ],
  False,
  TestID -> "BallHullQ-path-endpoints-open"
]

(* Input-form invariance: bare vertex list, list of InfraPoint, and a single
   multi-vertex InfraPoint all give the same ball hull. *)

VerificationTest[
  With[ { g = GridGraph[ { 6, 4 } ] },
    SameQ[
      FindBallHull[ g, { 1, 6, 22 } ],
      FindBallHull[ g, { InfraPoint[ { 1 } ], InfraPoint[ { 6 } ], InfraPoint[ { 22 } ] } ],
      FindBallHull[ g, InfraPoint[ { 1, 6, 22 } ] ] ] ],
  True,
  TestID -> "FindBallHull-input-form-invariance"
]

EndTestSection[]
