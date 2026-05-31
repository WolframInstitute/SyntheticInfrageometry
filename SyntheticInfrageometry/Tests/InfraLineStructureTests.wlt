BeginTestSection["InfraLineStructure"]

(* Consistency of a line system: every contiguous stretch of every line is a
   well-defined chosen path -- whenever two lines share an endpoint pair, the
   stretch between them agrees.  This is subpath-closedness checked directly
   from the stored lines, with no appeal to the (T2) verifier. *)

canonSeq[p_] := First @ Sort @ { p, Reverse @ p };

consistentLinesQ[lines_] :=
  Module[ { assoc = <||>, ok = True },
    Scan[
      line |-> Do[
        With[ { key = Sort @ { line[[ i ]], line[[ j ]] }, stretch = canonSeq @ line[[ i ;; j ]] },
          If[ KeyExistsQ[ assoc, key ] && assoc[ key ] =!= stretch, ok = False ];
          assoc[ key ] = stretch ],
        { i, Length[ line ] - 1 }, { j, i + 1, Length[ line ] } ],
      lines ];
    ok ];

coversAllPairsQ[ g_, lines_ ] :=
  Complement[
    Subsets[ Sort @ VertexList[ g ], { 2 } ],
    Union @@ ( Map[ Sort, Subsets[ #, { 2 } ] ] & /@ lines ) ] === { };

(* ===== Consistency: every produced system is subpath-closed ===== *)

VerificationTest[
  consistentLinesQ[ FindLineStructure[ PathGraph[ Range[ 5 ] ] ][ "Lines" ] ],
  True,
  TestID -> "FindLineStructure-consistent-PathGraph5"
]

VerificationTest[
  consistentLinesQ[ FindLineStructure[ CycleGraph[ 6 ] ][ "Lines" ] ],
  True,
  TestID -> "FindLineStructure-consistent-CycleGraph6"
]

VerificationTest[
  consistentLinesQ[ FindLineStructure[ GridGraph[ { 3, 3 } ] ][ "Lines" ] ],
  True,
  TestID -> "FindLineStructure-consistent-GridGraph33"
]

VerificationTest[
  consistentLinesQ[ FindLineStructure[ PetersenGraph[] ][ "Lines" ] ],
  True,
  TestID -> "FindLineStructure-consistent-Petersen"
]

(* ===== Coverage: every vertex pair lies on some line ===== *)

VerificationTest[
  And @@ ( coversAllPairsQ[ #, FindLineStructure[ # ][ "Lines" ] ] & /@
    { PathGraph[ Range[ 5 ] ], CycleGraph[ 6 ], GridGraph[ { 3, 3 } ], PetersenGraph[] } ),
  True,
  TestID -> "FindLineStructure-covers-all-pairs"
]

(* ===== C4: a valid 3-line structure covering all 6 pairs ===== *)

VerificationTest[
  Length @ FindLineStructure[ CycleGraph[ 4 ] ][ "Lines" ],
  3,
  TestID -> "FindLineStructure-C4-three-lines"
]

VerificationTest[
  With[ { ls = FindLineStructure[ CycleGraph[ 4 ] ] },
    { consistentLinesQ[ ls[ "Lines" ] ], coversAllPairsQ[ CycleGraph[ 4 ], ls[ "Lines" ] ] } ],
  { True, True },
  TestID -> "FindLineStructure-C4-consistent-and-covers"
]

(* ===== Geodetic graph: the path is forced to a single line = the diameter ===== *)

VerificationTest[
  canonSeq @ First @ FindLineStructure[ PathGraph[ Range[ 5 ] ] ][ "Lines" ],
  { 1, 2, 3, 4, 5 },
  TestID -> "FindLineStructure-PathGraph5-single-diameter-line"
]

VerificationTest[
  Length @ FindLineStructure[ PathGraph[ Range[ 5 ] ] ][ "Lines" ],
  1,
  TestID -> "FindLineStructure-PathGraph5-one-line"
]

(* ===== Wrapper accessors ===== *)

VerificationTest[
  With[ { ls = FindLineStructure[ CycleGraph[ 6 ] ] },
    ls[ "Realizations" ] === ls[ "Lines" ] && ls[ "First" ] === First @ ls[ "Lines" ] &&
      ls[[ 1 ]] === First @ ls[ "Lines" ] ],
  True,
  TestID -> "InfraLineStructure-accessors"
]

VerificationTest[
  With[ { ls = FindLineStructure[ PathGraph[ Range[ 5 ] ] ] },
    ls[ "Length" ] === ( ( Length[ # ] - 1 ) & /@ ls[ "Lines" ] ) ],
  True,
  TestID -> "InfraLineStructure-Length-edge-counts"
]

(* ===================== ConsistentPathSystemQ (T2) ===================== *)

(* ===== Every produced structure verifies as consistent ===== *)

VerificationTest[
  And @@ ( ConsistentPathSystemQ[ #, FindLineStructure[ # ] ] & /@
    { PathGraph[ Range[ 5 ] ], CycleGraph[ 6 ], GridGraph[ { 3, 3 } ], PetersenGraph[] } ),
  True,
  TestID -> "ConsistentPathSystemQ-T1-output-passes"
]

(* ===== Hand-broken line set on C6: two antipodal geodesics for {1,4} disagree ===== *)

VerificationTest[
  ConsistentPathSystemQ[ CycleGraph[ 6 ], { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  False,
  TestID -> "ConsistentPathSystemQ-C6-conflicting-diagonals-fail"
]

VerificationTest[
  ConsistentPathSystemQ[ CycleGraph[ 6 ], { { 1, 2, 3, 4 }, { 4, 5, 6 } } ],
  True,
  TestID -> "ConsistentPathSystemQ-C6-disjoint-pairs-pass"
]

(* ===== Essentiality: each maximal line's endpoint pair lies on exactly one line ===== *)

VerificationTest[
  And @@ Map[
    g |-> With[ { lines = FindLineStructure[ g ][ "Lines" ] },
      AllTrue[ lines, L |-> Count[ lines, o_ /; SubsetQ[ o, { First @ L, Last @ L } ] ] == 1 ] ],
    { PathGraph[ Range[ 5 ] ], CycleGraph[ 6 ], GridGraph[ { 3, 3 } ], PetersenGraph[] } ],
  True,
  TestID -> "ConsistentPathSystemQ-essentiality-endpoint-pair-unique"
]

(* ===== Association form: unfolded single path consistent; a swapped stretch fails ===== *)

VerificationTest[
  ConsistentPathSystemQ[ PathGraph[ Range[ 5 ] ],
    Association @ Catenate @ Table[
      Sort @ { #[[ i ]], #[[ j ]] } -> canonSeq @ #[[ i ;; j ]],
      { i, Length @ # - 1 }, { j, i + 1, Length @ # } ] & @ { 1, 2, 3, 4, 5 } ],
  True,
  TestID -> "ConsistentPathSystemQ-association-unfold-pass"
]

VerificationTest[
  ConsistentPathSystemQ[ CycleGraph[ 6 ],
    <| { 1, 2 } -> { 1, 2 }, { 2, 3 } -> { 2, 3 }, { 3, 4 } -> { 3, 4 },
       { 1, 3 } -> { 1, 2, 3 }, { 2, 4 } -> { 2, 1, 6, 5, 4 }, { 1, 4 } -> { 1, 2, 3, 4 } |> ],
  False,
  TestID -> "ConsistentPathSystemQ-association-swapped-stretch-fail"
]

(* ===================== Derived views + recovery (T3) ===================== *)

refGraphs = { PathGraph[ Range[ 5 ] ], CycleGraph[ 6 ], GridGraph[ { 3, 3 } ], PetersenGraph[] };

(* ===== Lines -> Paths: each line is recovered as the path between its endpoints ===== *)

VerificationTest[
  And @@ Map[
    g |-> With[ { ls = FindLineStructure[ g ] },
      AllTrue[ ls[ "Lines" ],
        L |-> canonSeq @ ls[ "Paths" ][ Sort @ { First @ L, Last @ L } ] === canonSeq @ L ] ],
    refGraphs ],
  True,
  TestID -> "InfraLineStructure-Paths-recovers-lines"
]

(* ===== Lines <-> Incidence: vertices on line k are recovered from the transpose ===== *)

VerificationTest[
  And @@ Map[
    g |-> With[ { ls = FindLineStructure[ g ] },
      With[ { lines = ls[ "Lines" ], inc = ls[ "Incidence" ] },
        Sort @ Keys @ inc === Sort @ VertexList @ g &&
          AllTrue[ Range @ Length @ lines,
            k |-> Sort @ Select[ Keys @ inc, MemberQ[ inc[ # ], k ] & ] === Sort @ lines[[ k ]] ] ] ],
    refGraphs ],
  True,
  TestID -> "InfraLineStructure-Incidence-transpose-roundtrip"
]

(* ===== ["Path",u,v] reproduces ["Paths"][{u,v}] for every pair ===== *)

VerificationTest[
  And @@ Map[
    g |-> With[ { ls = FindLineStructure[ g ] },
      AllTrue[ Keys @ ls[ "Paths" ],
        pair |-> ls[ "Path", First @ pair, Last @ pair ] === ls[ "Paths" ][ pair ] ] ],
    refGraphs ],
  True,
  TestID -> "InfraLineStructure-Path-reproduces-Paths"
]

(* ===== ["Path",u,v] is oriented u -> v (and v -> u is its reverse) ===== *)

VerificationTest[
  With[ { ls = FindLineStructure[ PathGraph[ Range[ 5 ] ] ] },
    { ls[ "Path", 2, 5 ], ls[ "Path", 5, 2 ] } ],
  { { 2, 3, 4, 5 }, { 5, 4, 3, 2 } },
  TestID -> "InfraLineStructure-Path-orientation"
]

(* ===== An interior pair on several lines yields the SAME stretch from each (C4 {3,4}) ===== *)

VerificationTest[
  With[ { lines = FindLineStructure[ CycleGraph[ 4 ] ][ "Lines" ] },
    With[ { onBoth = Select[ lines, ContainsAll[ #, { 3, 4 } ] & ] },
      Length @ onBoth >= 2 &&
        SameQ @@ Map[
          line |-> With[ { i = First @ FirstPosition[ line, 3 ], j = First @ FirstPosition[ line, 4 ] },
            canonSeq @ line[[ Min[ i, j ] ;; Max[ i, j ] ]] ],
          onBoth ] ] ],
  True,
  TestID -> "InfraLineStructure-multi-line-pair-same-stretch"
]

(* ===================== Methods + tie-break study (T4) ===================== *)

(* Every method reduces to an edge ranking -> exact 1 + 2^(-i) weighting, so each
   yields a consistent system whose lines are true unit-metric geodesics. *)

trueGeodesicLinesQ[ g_, lines_ ] :=
  AllTrue[ lines,
    L |-> AllTrue[ Partition[ L, 2, 1 ], EdgeQ[ g, UndirectedEdge @@ # ] & ] &&
      Length[ L ] - 1 == GraphDistance[ g, First @ L, Last @ L ] ];

methodList = { "Lexicographic", { "Random", 1 }, { "Random", 2 }, "Resistance",
  "Weight" -> ( First[ # ]^2 + Last[ # ] & ) };

(* ===== Each method gives a consistent, true-geodesic, fully covering system ===== *)

VerificationTest[
  AllTrue[
    Tuples[ { { CycleGraph[ 6 ], GridGraph[ { 3, 3 } ], PetersenGraph[] }, methodList } ],
    pair |-> With[ { g = First @ pair, lines = FindLineStructure[ First @ pair, Method -> Last @ pair ][ "Lines" ] },
      consistentLinesQ[ lines ] && trueGeodesicLinesQ[ g, lines ] && coversAllPairsQ[ g, lines ] ] ],
  True,
  TestID -> "FindLineStructure-all-methods-consistent-geodesic-covering"
]

(* ===== {"Random", seed} is deterministic given the seed ===== *)

VerificationTest[
  FindLineStructure[ GridGraph[ { 3, 4 } ], Method -> { "Random", 7 } ][ "Lines" ] ===
    FindLineStructure[ GridGraph[ { 3, 4 } ], Method -> { "Random", 7 } ][ "Lines" ],
  True,
  TestID -> "FindLineStructure-Random-seed-deterministic"
]

(* ===== The tie-break choice can change the structure (line count is method-dependent) ===== *)

VerificationTest[
  Length @ DeleteDuplicates @ Map[
    m |-> Length @ FindLineStructure[ GridGraph[ { 3, 4 } ], Method -> m ][ "Lines" ],
    methodList ] > 1,
  True,
  TestID -> "FindLineStructure-line-count-method-dependent"
]

(* ===== ["Coordinates"]: <|v -> {{line, offset}|>; the offset recovers v on the line ===== *)

VerificationTest[
  And @@ Map[
    g |-> With[ { ls = FindLineStructure[ g ] },
      With[ { lines = ls[ "Lines" ], coords = ls[ "Coordinates" ] },
        Sort @ Keys @ coords === Sort @ VertexList @ g &&
          AllTrue[ Keys @ coords,
            v |-> AllTrue[ coords[ v ], lp |-> lines[[ First @ lp, Last @ lp + 1 ]] === v ] ] ] ],
    refGraphs ],
  True,
  TestID -> "InfraLineStructure-Coordinates-offset-recovers-vertex"
]

(* ===== ["Coordinates"] line numbers agree with ["Incidence"] ===== *)

VerificationTest[
  With[ { ls = FindLineStructure[ GridGraph[ { 3, 3 } ] ] },
    ls[ "Incidence" ] === Map[ First, ls[ "Coordinates" ], { 2 } ] ],
  True,
  TestID -> "InfraLineStructure-Coordinates-matches-Incidence"
]

EndTestSection[]
