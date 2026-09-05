BeginTestSection["MethodLadder"]

(* ===================== The class-invariance contract ===================== *)

(* Method never changes the class: "Exhaustive", "Greedy" and "RandomGreedy" enumerate the same realisation set under All.  canon normalises a realisation whose order carries no information (a vertex set); a walk keeps its sequence *)

classInvariantQ[ call_, canon_ : Identity ] :=
  SameQ @@ ( Sort[ canon /@ call[ # ][ "Realizations" ] ] & /@ { "Exhaustive", "Greedy", "RandomGreedy" } )


(* ===================== Distance-matrix family ===================== *)

VerificationTest[
  classInvariantQ[ m |-> FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All, Method -> m ] ],
  True,
  TestID -> "FindInfraSegment-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> ExtendInfraSegment[ TorusGraph[ { 4, 5 } ], { 1, 2 }, Infinity, All, Method -> m ] ],
  True,
  TestID -> "ExtendInfraSegment-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraLine[ TorusGraph[ { 4, 5 } ], 1, 2, All, Method -> m ] ],
  True,
  TestID -> "FindInfraLine-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraRay[ TorusGraph[ { 4, 5 } ], 1, 2, All, Method -> m ] ],
  True,
  TestID -> "FindInfraRay-class-invariant-under-Method"
]

(* filed by MethodLadderConsistency T2 (2026-09-06), not yet fixed: "Exhaustive" canonicalises the orientation and keeps only the longest chains through each seed edge, "Greedy" emits every inextensible chain in both orientations *)
VerificationTest[
  classInvariantQ[ m |-> FindInfraParallel[ GridGraph[ { 5, 5 } ], Range[ 5 ], 13, All, Method -> m ] ],
  True,
  TestID -> "FindInfraParallel-class-invariant-under-Method"
]

(* two dead ends 11, 12 hang off 8 at distance 1 from the row 1..5: the chain 11-8-12 is inextensible in the level set but shorter than 6-7-8-11, so the per-seed maximum drops it *)
VerificationTest[
  With[ { g = Graph[ Join[
        UndirectedEdge @@@ Partition[ Range[ 5 ], 2, 1 ],
        UndirectedEdge @@@ Partition[ Range[ 6, 10 ], 2, 1 ],
        UndirectedEdge @@@ Transpose[ { Range[ 5 ], Range[ 6, 10 ] } ],
        { 11 <-> 8, 11 <-> 3, 12 <-> 8, 12 <-> 3 } ] ] },
    classInvariantQ[ m |-> FindInfraParallel[ g, Range[ 5 ], 8, All, Method -> m ] ] ],
  True,
  TestID -> "FindInfraParallel-class-invariant-dead-ends"
]


(* ===================== Walk family ===================== *)

VerificationTest[
  classInvariantQ[ m |-> FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 6, All,
    Properties -> { "Immersed" }, "StoppingCondition" -> 1, Method -> m ] ],
  True,
  TestID -> "FindInfraWalk-pointed-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 6, All, Properties -> { "Generic" }, Method -> m ] ],
  True,
  TestID -> "FindInfraWalk-two-point-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> ExtendInfraWalk[ GridGraph[ { 3, 3 } ], { 1, 2 }, 3, All, Method -> m ] ],
  True,
  TestID -> "ExtendInfraWalk-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraGeodesic[ GridGraph[ { 4, 4 } ], 1, 2, 4, All, Method -> m ] ],
  True,
  TestID -> "FindInfraGeodesic-pointed-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraGeodesic[ TorusGraph[ { 4, 5 } ], 1, 8, 2, 6, All, Method -> m ] ],
  True,
  TestID -> "FindInfraGeodesic-two-point-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> ExtendInfraGeodesic[ TorusGraph[ { 4, 5 } ], { 1, 2 }, 2, 3, All, Method -> m ] ],
  True,
  TestID -> "ExtendInfraGeodesic-class-invariant-under-Method"
]


(* ===================== Peel family ===================== *)

VerificationTest[
  classInvariantQ[ m |-> FindInfraShell[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All, Properties -> { "Separating" }, Method -> m ], Sort ],
  True,
  TestID -> "FindInfraShell-class-invariant-under-Method"
]

(* the band is columns 2 and 3; a minimal separator takes exactly one vertex per row, 2^4 of them *)
VerificationTest[
  With[ { call = m |-> FindInfraBisectingHyperplane[ GridGraph[ { 4, 4 } ], 1, 4, { -1, 1 }, All,
      Properties -> { "Separating" }, Method -> m ] },
    { classInvariantQ[ call, Sort ], Length @ call[ "Exhaustive" ][ "Realizations" ] } ],
  { True, 16 },
  TestID -> "FindInfraBisectingHyperplane-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraEllipticShell[ GridGraph[ { 4, 4 } ], { 6, 11 }, { 3, 4 }, All,
    Properties -> { "Separating" }, Method -> m ], Sort ],
  True,
  TestID -> "FindInfraEllipticShell-class-invariant-under-Method"
]


(* ===================== Pruning at Infinity is the whole class ===================== *)

(* the pruned exhaustive spec is accepted on every ladder symbol, and a keep-all cap changes nothing *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], t = TorusGraph[ { 4, 5 } ] },
    AllTrue[
      { m |-> FindInfraSegment[ g, 1, 16, All, Method -> m ],
        m |-> ExtendInfraSegment[ g, { 6, 7 }, 2, All, Method -> m ],
        m |-> FindInfraLine[ g, 1, 2, All, Method -> m ],
        m |-> FindInfraRay[ g, 6, 7, All, Method -> m ],
        m |-> FindInfraParallel[ g, Range[ 4 ], 10, All, Method -> m ],
        m |-> FindInfraWalk[ g, 1, 4, All, Method -> m ],
        m |-> ExtendInfraWalk[ g, { 1, 2 }, 2, All, Method -> m ],
        m |-> FindInfraGeodesic[ g, 1, 2, 4, All, Method -> m ],
        m |-> ExtendInfraGeodesic[ g, { 6, 7 }, Infinity, 2, All, Method -> m ],
        m |-> FindInfraShell[ g, 6, { 1, 2 }, All, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraBisectingHyperplane[ g, 1, 4, { -1, 1 }, All, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraEllipticShell[ g, { 6, 11 }, { 3, 4 }, All, Properties -> { "Separating" }, Method -> m ] },
      call |-> Sort[ Sort /@ call[ { "Exhaustive", "Pruning" -> Infinity } ][ "Realizations" ] ] ===
               Sort[ Sort /@ call[ "Exhaustive" ][ "Realizations" ] ] ] ],
  True,
  TestID -> "MethodLadder-Pruning-Infinity-is-the-whole-class"
]

EndTestSection[]
