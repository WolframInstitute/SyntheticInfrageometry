(* ===================== InfraPolyline auto-flatten ===================== *)

VerificationTest[
  InfraPolyline[ { InfraPolyline[ { { InfraSegment[ { { 1, 2 } } ] } } ],
                   InfraPolyline[ { { InfraSegment[ { { 2, 3 } } ] } } ] } ],
  InfraPolyline[ { { InfraSegment[ { { 1, 2 } } ] }, { InfraSegment[ { { 2, 3 } } ] } } ],
  TestID -> "InfraPolyline-auto-flatten"
]


(* ===================== FindInfraPolylineSubdivision: trivial cases ===================== *)

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 5 ], { } ],
  InfraPolyline[ { { } } ],
  TestID -> "FindInfraPolylineSubdivision-empty-path"
]

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 5 ], { 3 } ],
  InfraPolyline[ { { } } ],
  TestID -> "FindInfraPolylineSubdivision-single-vertex-path"
]


(* ===================== FindInfraPolylineSubdivision: no constraint ===================== *)

(* On PathGraph[1..6], the whole geodesic 1-2-3-4-5-6 is a single shortest
   path when MaxLength = Infinity, so one leg. *)

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 6 ], Range[ 6 ] ],
  InfraPolyline[ { { InfraSegment[ { Range[ 6 ] } ] } } ],
  TestID -> "FindInfraPolylineSubdivision-infinite-maxlength"
]


(* ===================== FindInfraPolylineSubdivision: max-length chunking ===================== *)

(* On PathGraph[1..11], geodesic 1..11 with MaxLength = 3 yields legs of
   lengths 3, 3, 3, 1 (knots at indices 1, 4, 7, 10, 11). *)

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ],
  InfraPolyline[ { {
    InfraSegment[ { { 1, 2, 3, 4 } } ],
    InfraSegment[ { { 4, 5, 6, 7 } } ],
    InfraSegment[ { { 7, 8, 9, 10 } } ],
    InfraSegment[ { { 10, 11 } } ]
  } } ],
  TestID -> "FindInfraPolylineSubdivision-maxlength-3-on-PathGraph-11"
]


(* Shared-endpoint invariant: consecutive legs of the result must agree
   on their joining vertex.  *)

VerificationTest[
  With[ { poly = First @ First @ FindInfraPolylineSubdivision[
      PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] },
    AllTrue[ Partition[ poly, 2, 1 ],
      pair |-> Last[ pair[[ 1, 1, 1 ]] ] === First[ pair[[ 2, 1, 1 ]] ] ] ],
  True,
  TestID -> "FindInfraPolylineSubdivision-shared-endpoints"
]


(* Non-geodesic walk: detour 1-2-3-4-3-2-1 on PathGraph[1..4] is not a
   shortest path past index 4, so a break is forced at the apex.        *)

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 4 ], { 1, 2, 3, 4, 3, 2, 1 } ],
  InfraPolyline[ { {
    InfraSegment[ { { 1, 2, 3, 4 } } ],
    InfraSegment[ { { 4, 3, 2, 1 } } ]
  } } ],
  TestID -> "FindInfraPolylineSubdivision-detour-break"
]


(* ===================== InfraPolylineQ ===================== *)

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 11 ],
    FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] ],
  True,
  TestID -> "InfraPolylineQ-wrapped-true"
]

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ],
    { InfraSegment[ { { 1, 2 } } ], InfraSegment[ { { 2, 3, 4 } } ], InfraSegment[ { { 4, 5 } } ] } ],
  True,
  TestID -> "InfraPolylineQ-bare-list-true"
]

(* Inconsistent: shared-endpoint invariant violated. *)

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ],
    { InfraSegment[ { { 1, 2 } } ], InfraSegment[ { { 3, 4 } } ] } ],
  False,
  TestID -> "InfraPolylineQ-broken-share"
]

(* Inconsistent: a leg whose stored vertex sequence is not a path in graph. *)

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ],
    { InfraSegment[ { { 1, 3 } } ] } ],
  False,
  TestID -> "InfraPolylineQ-leg-not-in-graph"
]

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ], { } ],
  True,
  TestID -> "InfraPolylineQ-empty"
]


(* ===================== Visualisation ===================== *)

(* InfraSceneHighlight returns a Graph and visits both the path edges and
   the knot vertices (added in $InfraPointColor on top of the path). *)

VerificationTest[
  Head @ InfraSceneHighlight[ PathGraph @ Range[ 11 ],
    { FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] } ],
  Graph,
  TestID -> "InfraSceneHighlight-accepts-InfraPolyline"
]
