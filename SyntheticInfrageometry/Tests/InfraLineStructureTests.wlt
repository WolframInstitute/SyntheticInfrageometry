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

EndTestSection[]
