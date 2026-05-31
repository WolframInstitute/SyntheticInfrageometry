BeginTestSection["Hulls"]

(* FindLineHull is a closure operator: extensive, idempotent, monotone, and
   it contains the segment hull (since I(u,v) subset L(u,v)). *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], s = { 1, 6 } },
    SubsetQ[ FindLineHull[ g, s ], Union @ s ] ],
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
    SubsetQ[ FindLineHull[ g, s ], FindSegmentHull[ g, s ] ] ],
  True,
  TestID -> "segment-hull-subset-of-line-hull"
]

(* PathGraph has a single maximal line (the whole path), so the line hull of
   any two distinct vertices is the entire path -- a universal line. *)

VerificationTest[
  FindLineHull[ PathGraph @ Range[ 5 ], { 2, 4 } ],
  Range[ 5 ],
  TestID -> "FindLineHull-path-universal"
]

VerificationTest[
  FindLineHull[ PathGraph @ Range[ 5 ], { 2, 3 } ],
  Range[ 5 ],
  TestID -> "FindLineHull-path-universal-adjacent"
]

(* C4: the two geodesics through antipodes 1, 3 close up to the whole cycle. *)

VerificationTest[
  FindLineHull[ CycleGraph[ 4 ], { 1, 3 } ],
  { 1, 2, 3, 4 },
  TestID -> "FindLineHull-C4-antipodal"
]

(* A singleton is its own line hull (no line meets it in >= 2 vertices). *)

VerificationTest[
  FindLineHull[ GridGraph[ { 3, 3 } ], { 5 } ],
  { 5 },
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
    SubsetQ[ FindLineHull[ g, { 2, 8 } ], FindLineHull[ g, { 2, 8 }, "LineStructure" -> ls ] ] ],
  True,
  TestID -> "FindLineHull-structure-subset-of-full"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { ls = FindLineStructure[ g ], full = FindLineHull[ g, { 1, 2 } ] },
    { via = FindLineHull[ g, { 1, 2 }, "LineStructure" -> ls ] },
    via =!= full && SubsetQ[ full, via ] ],
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
  { 1, 2, 3, 4 },
  TestID -> "FindLineHull-bare-line-family"
]

(* Segment side: the "LineStructure" option closes under chosen-geodesic
   stretches.  Bounded above by both the unrestricted segment hull (one chosen
   geodesic vs all geodesics) and the line hull under the same family (stretch
   vs whole line); idempotent (unique fixed point). *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    SubsetQ[ FindSegmentHull[ g, { 1, 9 } ], FindSegmentHull[ g, { 1, 9 }, "LineStructure" -> ls ] ] ],
  True,
  TestID -> "FindSegmentHull-structure-subset-of-full"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] }, { ls = FindLineStructure[ g ] },
    SubsetQ[ FindLineHull[ g, { 1, 9 }, "LineStructure" -> ls ],
             FindSegmentHull[ g, { 1, 9 }, "LineStructure" -> ls ] ] ],
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

EndTestSection[]
