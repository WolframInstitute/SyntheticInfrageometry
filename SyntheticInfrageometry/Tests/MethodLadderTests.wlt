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

(* the parallels through the centre of the 5 x 5 grid in the level set of its first row: one chain, the middle row *)
VerificationTest[
  classInvariantQ[ m |-> FindInfraParallel[ GridGraph[ { 5, 5 } ], Range[ 5 ], 13, All, Method -> m ] ],
  True,
  TestID -> "FindInfraParallel-class-invariant-under-Method"
]

(* two dead ends 11, 12 hang off 8 at distance 1 from the row 1..5: the chain 11-8-12 is inextensible in the level set but shorter than 6-7-8-11, so a longest-only sweep would drop it -- the class holds all six *)
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

(* the corner polygon is the product of its sides' geodesic classes: the diagonal side 9 -> 1 of the 3 x 3 grid has six geodesics, the other two one each *)
VerificationTest[
  With[ { call = m |-> FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All, Method -> m ] },
    { classInvariantQ[ call ], Length @ call[ "Exhaustive" ][ "Realizations" ] } ],
  { True, 6 },
  TestID -> "FindInfraPolygon-class-invariant-under-Method"
]

VerificationTest[
  classInvariantQ[ m |-> FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All, Method -> m ] ],
  True,
  TestID -> "FindInfraTriangle-class-invariant-under-Method"
]

(* a bounded count streams n geodesics per side and reads the first members of their product: prefixes of length n multiply to at least Min[n, |class|] polygons, so a strict count is exact under every Method and a soft count past the class returns the class *)
VerificationTest[
  Table[ Length @ FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, n, Method -> m ][ "Realizations" ],
    { m, { "Exhaustive", "Greedy", "RandomGreedy" } }, { n, { 1, 4, UpTo[ 10 ] } } ],
  ConstantArray[ { 1, 4, 6 }, 3 ],
  TestID -> "FindInfraPolygon-bounded-count-is-exact-under-Method"
]

(* four diagonal sides of the 4 x 4 grid with twenty geodesics each, 160 000 polygons: a strict count streams that many distinct members without forming the product *)
VerificationTest[
  Table[ With[ { reps = FindInfraPolygon[ GridGraph[ { 4, 4 } ], { 1, 16, 4, 13 }, 50, Method -> m ][ "Realizations" ] },
      { Length @ reps, DuplicateFreeQ @ reps, AllTrue[ reps, InfraPolygonQ[ GridGraph[ { 4, 4 } ], # ] & ] } ],
    { m, { "Exhaustive", "Greedy", "RandomGreedy" } } ],
  ConstantArray[ { 50, True, True }, 3 ],
  TestID -> "FindInfraPolygon-strict-count-streams-off-the-product"
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

(* the peel from the centre of the 5 x 5 grid: sixteen minimal separators, and the lazy peel reaches each subset once -- without its visited set this ran minutes *)
VerificationTest[
  With[ { call = m |-> FindInfraShell[ GridGraph[ { 5, 5 } ], 13, { 1, 2 }, All, Properties -> { "Separating" }, Method -> m ] },
    { classInvariantQ[ call, Sort ], Length @ call[ "Exhaustive" ][ "Realizations" ] } ],
  { True, 16 },
  TestID -> "FindInfraShell-5x5-class-invariant-under-Method"
]


(* ===================== Circle family ===================== *)

(* the band {2, 4} around the centre of the 9 x 9 grid: one pool atom carrying sixteen shortest separating circles, the class under every Method *)
VerificationTest[
  With[ { call = m |-> FindInfraCircle[ GridGraph[ { 9, 9 } ], 41, { 2, 4 }, All, Method -> m ] },
    { classInvariantQ[ call ], Length @ call[ "Exhaustive" ][ "Realizations" ] } ],
  { True, 16 },
  TestID -> "FindInfraCircle-pool-class-invariant-under-Method"
]

(* off the default Properties the family comes from the length sweep, which every Method runs alike *)
VerificationTest[
  classInvariantQ[ m |-> FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All, Properties -> { "Separating" }, Method -> m ] ],
  True,
  TestID -> "FindInfraCircle-sweep-class-invariant-under-Method"
]


(* ===================== Automatic is the deterministic descent ===================== *)

(* on every ladder symbol a count-less call resolves to "Greedy": the same witness twice without a seed, and the explicit "Greedy" witness *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], t = TorusGraph[ { 4, 5 } ] },
    AllTrue[
      { m |-> FindInfraSegment[ g, 1, 16, Method -> m ],
        m |-> ExtendInfraSegment[ g, { 6, 7 }, 2, Method -> m ],
        m |-> FindInfraLine[ t, 1, 2, Method -> m ],
        m |-> FindInfraRay[ g, 6, 7, Method -> m ],
        m |-> FindInfraParallel[ g, Range[ 4 ], 10, Method -> m ],
        m |-> FindInfraWalk[ g, 1, 4, Method -> m ],
        m |-> FindInfraWalk[ g, 1, InfraPoint[ 16 ], { 6 }, Method -> m ],
        m |-> ExtendInfraWalk[ g, { 1, 2 }, 2, Method -> m ],
        m |-> FindInfraGeodesic[ g, 1, 2, 4, Method -> m ],
        m |-> ExtendInfraGeodesic[ g, { 6, 7 }, Infinity, 2, Method -> m ],
        m |-> FindInfraShell[ g, 6, { 1, 2 }, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraBisectingHyperplane[ g, 1, 4, { -1, 1 }, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraEllipticShell[ g, { 6, 11 }, { 3, 4 }, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraCircle[ g, 6, { 1, 2 }, Method -> m ],
        m |-> FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, Method -> m ],
        m |-> FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, Method -> m ] },
      call |-> call[ Automatic ] === call[ Automatic ] === call[ "Greedy" ] ] ],
  True,
  TestID -> "MethodLadder-Automatic-is-Greedy-on-every-symbol"
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
        m |-> FindInfraEllipticShell[ g, { 6, 11 }, { 3, 4 }, All, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraCircle[ g, 6, { 1, 2 }, All, Method -> m ],
        m |-> FindInfraCircle[ g, 6, { 1, 2 }, All, Properties -> { "Separating" }, Method -> m ],
        m |-> FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All, Method -> m ],
        m |-> FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All, Method -> m ] },
      call |-> Sort[ Sort /@ call[ { "Exhaustive", "Pruning" -> Infinity } ][ "Realizations" ] ] ===
               Sort[ Sort /@ call[ "Exhaustive" ][ "Realizations" ] ] ] ],
  True,
  TestID -> "MethodLadder-Pruning-Infinity-is-the-whole-class"
]

EndTestSection[]
