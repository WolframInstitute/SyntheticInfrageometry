BeginTestSection["InfraPolyline"]

geodesicGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
walkSequence  = WolframInstitute`SyntheticInfrageometry`PackageScope`walkSequence;
polylineToKnots = WolframInstitute`SyntheticInfrageometry`PackageScope`polylineToKnots;

(* a polyline is its geodesic legs: a List of directed path graphs on the substrate, consecutive legs sharing their knot *)


(* ===================== FindInfraPolylineSubdivision: trivial cases ===================== *)

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 5 ], { } ],
  { },
  TestID -> "FindInfraPolylineSubdivision-empty-path"
]

VerificationTest[
  FindInfraPolylineSubdivision[ PathGraph @ Range[ 5 ], { 3 } ],
  { },
  TestID -> "FindInfraPolylineSubdivision-single-vertex-path"
]


(* ===================== FindInfraPolylineSubdivision: no constraint ===================== *)

(* On PathGraph[1..6], the whole geodesic 1-2-3-4-5-6 is a single shortest
   path when MaxLength = Infinity, so one leg. *)

VerificationTest[
  walkSequence /@ FindInfraPolylineSubdivision[ PathGraph @ Range[ 6 ], Range[ 6 ] ],
  { Range[ 6 ] },
  TestID -> "FindInfraPolylineSubdivision-infinite-maxlength"
]


(* ===================== FindInfraPolylineSubdivision: max-length chunking ===================== *)

(* On PathGraph[1..11], geodesic 1..11 with MaxLength = 3 yields legs of
   lengths 3, 3, 3, 1 (knots at indices 1, 4, 7, 10, 11). *)

VerificationTest[
  walkSequence /@ FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ],
  { { 1, 2, 3, 4 }, { 4, 5, 6, 7 }, { 7, 8, 9, 10 }, { 10, 11 } },
  TestID -> "FindInfraPolylineSubdivision-maxlength-3-on-PathGraph-11"
]

VerificationTest[
  polylineToKnots @ FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ],
  { 1, 4, 7, 10, 11 },
  TestID -> "FindInfraPolylineSubdivision-knots-are-the-leg-ends"
]


(* Shared-endpoint invariant: consecutive legs of the result must agree
   on their joining vertex.  *)

VerificationTest[
  With[ { legs = walkSequence /@ FindInfraPolylineSubdivision[
      PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] },
    AllTrue[ Partition[ legs, 2, 1 ], pair |-> Last[ pair[[ 1 ]] ] === First[ pair[[ 2 ]] ] ] ],
  True,
  TestID -> "FindInfraPolylineSubdivision-shared-endpoints"
]


(* Non-geodesic walk: detour 1-2-3-4-3-2-1 on PathGraph[1..4] is not a
   shortest path past index 4, so a break is forced at the apex.        *)

VerificationTest[
  walkSequence /@ FindInfraPolylineSubdivision[ PathGraph @ Range[ 4 ], { 1, 2, 3, 4, 3, 2, 1 } ],
  { { 1, 2, 3, 4 }, { 4, 3, 2, 1 } },
  TestID -> "FindInfraPolylineSubdivision-detour-break"
]


(* ===================== InfraPolylineQ ===================== *)

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 11 ],
    FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] ],
  True,
  TestID -> "InfraPolylineQ-constructor-output-true"
]

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ], geodesicGraph /@ { { 1, 2 }, { 2, 3, 4 }, { 4, 5 } } ],
  True,
  TestID -> "InfraPolylineQ-legs-true"
]

(* Inconsistent: shared-endpoint invariant violated. *)

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ], geodesicGraph /@ { { 1, 2 }, { 3, 4 } } ],
  False,
  TestID -> "InfraPolylineQ-broken-share"
]

(* Inconsistent: a leg whose vertex sequence is not a path in graph. *)

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ], { geodesicGraph @ { 1, 3 } } ],
  False,
  TestID -> "InfraPolylineQ-leg-not-in-graph"
]

VerificationTest[
  InfraPolylineQ[ PathGraph @ Range[ 5 ], { } ],
  True,
  TestID -> "InfraPolylineQ-empty"
]


(* ===================== Visualisation ===================== *)

(* a polyline is a legal HighlightGraph argument and draws through InfraSceneHighlight *)

VerificationTest[
  With[ { poly = FindInfraPolylineSubdivision[ PathGraph @ Range[ 11 ], Range[ 11 ], "MaxLength" -> 3 ] },
    { Head @ HighlightGraph[ PathGraph @ Range[ 11 ], poly ],
      Head @ InfraSceneHighlight[ PathGraph @ Range[ 11 ], { poly } ] } ],
  { Graph, Graph },
  TestID -> "InfraSceneHighlight-accepts-polyline"
]

EndTestSection[]
