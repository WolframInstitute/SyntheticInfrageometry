(* ===================== InfraPolyline auto-flatten ===================== *)

VerificationTest[
  InfraPolyline[ { InfraPolyline[ { { InfraSegment[ { { 1, 2 } } ] } } ],
                   InfraPolyline[ { { InfraSegment[ { { 2, 3 } } ] } } ] } ],
  InfraPolyline[ { { InfraSegment[ { { 1, 2 } } ] }, { InfraSegment[ { { 2, 3 } } ] } } ],
  TestID -> "InfraPolyline-auto-flatten"
]


(* ===================== FindPolylineSubdivision: trivial cases ===================== *)

VerificationTest[
  FindPolylineSubdivision[ PathGraph @ Range[ 5 ], { } ],
  InfraPolyline[ { { } } ],
  TestID -> "FindPolylineSubdivision-empty-path"
]

VerificationTest[
  FindPolylineSubdivision[ PathGraph @ Range[ 5 ], { 3 } ],
  InfraPolyline[ { { } } ],
  TestID -> "FindPolylineSubdivision-single-vertex-path"
]


(* ===================== FindPolylineSubdivision: no constraint ===================== *)

(* On PathGraph[1..6], the whole geodesic 1-2-3-4-5-6 is a single shortest
   path when MaxLength = Infinity, so one leg. *)

VerificationTest[
  FindPolylineSubdivision[ PathGraph @ Range[ 6 ], Range[ 6 ] ],
  InfraPolyline[ { { InfraSegment[ { Range[ 6 ] } ] } } ],
  TestID -> "FindPolylineSubdivision-infinite-maxlength"
]


(* ===================== FindPolylineSubdivision: max-length chunking ===================== *)

(* On PathGraph[1..11], geodesic 1..11 with MaxLength = 3 yields legs of
   lengths 3, 3, 3, 1 (knots at indices 1, 4, 7, 10, 11). *)

VerificationTest[
  FindPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ],
  InfraPolyline[ { {
    InfraSegment[ { { 1, 2, 3, 4 } } ],
    InfraSegment[ { { 4, 5, 6, 7 } } ],
    InfraSegment[ { { 7, 8, 9, 10 } } ],
    InfraSegment[ { { 10, 11 } } ]
  } } ],
  TestID -> "FindPolylineSubdivision-maxlength-3-on-PathGraph-11"
]


(* Shared-endpoint invariant: consecutive legs of the result must agree
   on their joining vertex.  *)

VerificationTest[
  With[ { poly = First @ First @ FindPolylineSubdivision[
      PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] },
    AllTrue[ Partition[ poly, 2, 1 ],
      pair |-> Last[ pair[[ 1, 1, 1 ]] ] === First[ pair[[ 2, 1, 1 ]] ] ] ],
  True,
  TestID -> "FindPolylineSubdivision-shared-endpoints"
]


(* Non-geodesic walk: detour 1-2-3-4-3-2-1 on PathGraph[1..4] is not a
   shortest path past index 4, so a break is forced at the apex.        *)

VerificationTest[
  FindPolylineSubdivision[ PathGraph @ Range[ 4 ], { 1, 2, 3, 4, 3, 2, 1 } ],
  InfraPolyline[ { {
    InfraSegment[ { { 1, 2, 3, 4 } } ],
    InfraSegment[ { { 4, 3, 2, 1 } } ]
  } } ],
  TestID -> "FindPolylineSubdivision-detour-break"
]


(* ===================== PolylineQ ===================== *)

VerificationTest[
  PolylineQ[ PathGraph @ Range[ 11 ],
    FindPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] ],
  True,
  TestID -> "PolylineQ-wrapped-true"
]

VerificationTest[
  PolylineQ[ PathGraph @ Range[ 5 ],
    { InfraSegment[ { { 1, 2 } } ], InfraSegment[ { { 2, 3, 4 } } ], InfraSegment[ { { 4, 5 } } ] } ],
  True,
  TestID -> "PolylineQ-bare-list-true"
]

(* Inconsistent: shared-endpoint invariant violated. *)

VerificationTest[
  PolylineQ[ PathGraph @ Range[ 5 ],
    { InfraSegment[ { { 1, 2 } } ], InfraSegment[ { { 3, 4 } } ] } ],
  False,
  TestID -> "PolylineQ-broken-share"
]

(* Inconsistent: a leg whose stored vertex sequence is not a path in graph. *)

VerificationTest[
  PolylineQ[ PathGraph @ Range[ 5 ],
    { InfraSegment[ { { 1, 3 } } ] } ],
  False,
  TestID -> "PolylineQ-leg-not-in-graph"
]

VerificationTest[
  PolylineQ[ PathGraph @ Range[ 5 ], { } ],
  True,
  TestID -> "PolylineQ-empty"
]


(* ===================== Visualisation ===================== *)

(* InfraSceneHighlight returns a Graph and visits both the path edges and
   the knot vertices (added in $InfraPointColor on top of the path). *)

VerificationTest[
  Head @ InfraSceneHighlight[ PathGraph @ Range[ 11 ],
    { FindPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] } ],
  Graph,
  TestID -> "InfraSceneHighlight-accepts-InfraPolyline"
]
