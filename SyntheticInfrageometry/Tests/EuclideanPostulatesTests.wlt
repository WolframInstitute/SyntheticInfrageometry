BeginTestSection["EuclideanPostulates"]

walkGraph       = WolframInstitute`SyntheticInfrageometry`PackageScope`walkGraph;
geodesicGraph      = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
geodesicCycleGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicCycleGraph;
walkSequence       = WolframInstitute`SyntheticInfrageometry`PackageScope`walkSequence;
infraSpread        = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;
infraNumReps       = WolframInstitute`SyntheticInfrageometry`PackageScope`infraNumReps;
infraEdgeMultiset  = WolframInstitute`SyntheticInfrageometry`PackageScope`infraEdgeMultiset;
toDensity          = WolframInstitute`SyntheticInfrageometry`PackageScope`toDensity;
closedWalkGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`closedWalkGraph;
walkSeq[ w_Graph ] := Last /@ VertexList[ w ]
walkSeqs[ ws_List ] := walkSeq /@ ws

(* ===== FindInfraPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g]},
      MemberQ[VertexList[g], pt]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindInfraPoint[g, 3]},
      Length @ pts == 3 && DuplicateFreeQ[(pts)] && SubsetQ[VertexList[g], (pts)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], (FindInfraPoint[g, 1, "From" -> "Center"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], (FindInfraPoint[g, 1, "From" -> "Periphery"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-periphery"
]

(* {"Center", 0} runs zero iterations -- identical pool to plain "Center". *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort[FindInfraPoint[g, All, "From" -> {"Center", 0}]] ===
      Sort[FindInfraPoint[g, All, "From" -> "Center"]]
  ],
  True,
  TestID -> "FindInfraPoint-iterated-center-cap0-equals-center"
]

(* On a graph with a dominating center the orbit is already a fixed point, so the
   iterated (uncapped) pool is the center of that fixed point -- a subset of the graph. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    SubsetQ[VertexList[g], FindInfraPoint[g, All, "From" -> {"Center", Infinity}]]
  ],
  True,
  TestID -> "FindInfraPoint-iterated-center-stabilizes-in-graph"
]

(* A disconnected input is already a shattered region: the pool is its whole vertex set. *)
VerificationTest[
  With[{g = GraphDisjointUnion[PathGraph[{1, 2, 3}], PathGraph[{4, 5, 6}]]},
    Sort[FindInfraPoint[g, All, "From" -> {"Center", Infinity}]] === VertexList[g]
  ],
  True,
  TestID -> "FindInfraPoint-iterated-center-disconnected-region"
]

(* An unrecognised "From" selector is refused rather than silently read as the
   whole vertex pool -- the retired "MinCurvature" / "MaxCurvature" names are the
   case that matters, since a reader copying them from older prose otherwise gets
   a random draw and no indication the selector did nothing. *)
VerificationTest[
  FindInfraPoint[GridGraph[{5, 5}], All, "From" -> "MinCurvature"],
  $Failed,
  {FindInfraPoint::badfrom},
  TestID -> "FindInfraPoint-badfrom-retired-selector"
]

(* an atom that is not a vertex of the graph is not a pool either *)
VerificationTest[
  FindInfraPoint[GridGraph[{5, 5}], All, "From" -> 999],
  $Failed,
  {FindInfraPoint::badfrom},
  TestID -> "FindInfraPoint-badfrom-non-vertex"
]

VerificationTest[
  FindInfraPoint[GridGraph[{5, 5}], "From" -> "Nonsense"],
  $Failed,
  {FindInfraPoint::badfrom},
  TestID -> "FindInfraPoint-badfrom-no-count-form"
]

(* Refusing a legitimate selector would be worse than the silence it replaces:
   every admissible "From" shape must still produce a pool. *)
VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    FreeQ[
      FindInfraPoint[g, All, "From" -> #] & /@
        {All, "Random", "Center", "Periphery", {"Center", 0}, {"Center", Infinity},
         7, 1 -> 2, {2, 3, 4}, 9, <| 2 -> 1, 5 -> 1, 7 -> 1 |>,
         <| 3 -> 1, 4 -> 1 |>},
      $Failed]
  ],
  True,
  TestID -> "FindInfraPoint-From-vocabulary-not-refused"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (FindInfraPoint[g, 2, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindInfraPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    With[{vs = FindInfraPoint[g, 4, "Distance" -> "Max"]},
      Min[GraphDistance[g, #[[1]], #[[2]]] & /@ Subsets[vs, {2}]] == 5
    ]
  ],
  True,
  TestID -> "FindInfraPoint-Max-maximizes-minimum-gap"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    With[{spread = FindInfraPoint[g, 4, "Distance" -> "Spread"],
          corners = {1, 6, 31, 36}},
      With[{spreadDists = GraphDistance[g, #[[1]], #[[2]]] & /@ Subsets[spread, {2}],
            cornerDists = GraphDistance[g, #[[1]], #[[2]]] & /@ Subsets[corners, {2}]},
        Min[spreadDists] == 5 && Variance[spreadDists] <= Variance[cornerDists]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-Spread-minimizes-variance-at-optimal-gap"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g, 1, "From" -> {2, 3, 4}]},
      Length @ pt == 1 && SubsetQ[{2, 3, 4}, (pt)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (FindInfraPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindInfraPoint-vertex-list-with-distance"
]

VerificationTest[
  FindInfraPoint[PathGraph[Range[3]], 10],
  $Failed,
  TestID -> "FindInfraPoint-exact-fails-when-too-few"
]

VerificationTest[
  With[{pts = FindInfraPoint[PathGraph[Range[3]], UpTo[10]]},
    Length @ pts == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], (pts)]
  ],
  True,
  TestID -> "FindInfraPoint-upto-returns-available"
]

VerificationTest[
  FindInfraPoint[PathGraph[Range[3]], 3, "Distance" -> 5],
  $Failed,
  TestID -> "FindInfraPoint-exact-fails-impossible-distance"
]

VerificationTest[
  With[{g = PathGraph[Range[7]]},
    With[{pt = FindInfraPoint[g, 1, "From" -> 3 -> 2]},
      Length @ pt == 1 && GraphDistance[g, 3, First[pt]] == 2
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (FindInfraPoint[g, UpTo[20], "From" -> 1 -> {2, 3}])},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = (FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"])},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-max-distance"
]

VerificationTest[
  With[ { g = PetersenGraph[] },
    With[ { pts = FindInfraPoint[ g, 3 ] },
      Length[ pts ] === 3 && SubsetQ[ VertexList @ g, pts ] ] ],
  True,
  TestID -> "FindInfraPoint-returns-list-of-vertices"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (FindInfraPoint[g, UpTo[VertexCount[g]],
      "From" -> <| 1 -> 1, 16 -> 1 |> -> 3])},
      AllTrue[pts, GraphDistance[g, 1, #] == 3 && GraphDistance[g, 16, #] == 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multi-anchor-intersection"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ (FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> <| 2 -> 1, 5 -> 1, 7 -> 1 |>])
  ],
  {2, 5, 7},
  TestID -> "FindInfraPoint-multi-anchor-pool-no-distance"
]

(* ===== FindInfraSegment ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    infraSpread @ FindInfraSegment[g, 1, 5, All]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = infraSpread @ FindInfraSegment[g, 1, 3, All]},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindInfraSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = infraSpread @ FindInfraSegment[g, 1, 9, All]},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (SelectInfraWalk[g, infraSpread @ FindInfraSegment[g, 1, 9, All],All, "From" -> "Center", "Metric" -> "Frechet"])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraWalk-Center-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosest[g, infraSpread @ FindInfraSegment[g, 1, 9, All], {1, 9}]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EmbeddingClosest"
]

VerificationTest[
  FindInfraSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  { },
  TestID -> "FindInfraSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (SelectInfraWalk[g, infraSpread @ FindInfraSegment[g, 1, 9, All],All, "From" -> "Center", "Metric" -> "Hausdorff"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraWalk-Center-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (SelectInfraWalk[g, infraSpread @ FindInfraSegment[g, 1, 9, All],All, "From" -> "Periphery"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraWalk-Periphery"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosest[g, {1, 9}] @ SelectInfraWalk[g, All, "From" -> "Center"] @
        (infraSpread @ FindInfraSegment[g, 1, 9, All])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = infraSpread @ FindInfraSegment[g, 1, 9, UpTo[2]]},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-upto-soft-cap"
]

(* ===== FindInfraSegment geodesic-DAG default form ===== *)

(* All over vertex endpoints returns the compact geodesic interval DAG, a bare
   Graph; its invariants are read off the DAG with Wolfram's own operations --
   the family size, the common geodesic length, and the interval vertex set. *)
VerificationTest[
  With[{s = FindInfraSegment[GridGraph[{3, 3}], 1, 9, All]},
    {GraphQ[s], DirectedGraphQ[s], infraNumReps @ s,
     Max @ Values @ WolframInstitute`SyntheticInfrageometry`PackageScope`dagLayers[s],
     Sort @ VertexList[s]}
  ],
  {True, True, 6, 4, Range[9]},
  TestID -> "FindInfraSegment-default-is-geodesic-dag"
]

(* the DAG-form density equals the density of the enumerated family *)
VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    {s = FindInfraSegment[g, 1, 9, All]},
    InfraDensity[g, s] === InfraDensity[g, geodesicGraph /@ infraSpread @ s]
  ],
  True,
  TestID -> "FindInfraSegment-dag-density-matches-enumeration"
]

(* the DAG stands for the whole family, and a bounded count is a prefix of it *)
VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    {Length[infraSpread @ FindInfraSegment[g, 1, 9, All]],
     Length[FindInfraSegment[g, 1, 9, UpTo[3]]]}
  ],
  {6, 3},
  TestID -> "FindInfraSegment-dag-realizations-bridge"
]

VerificationTest[
  With[{r = FindInfraSegment[GridGraph[{3, 3}], 1, 9, 3]},
    MatchQ[r, {_Graph, _Graph, _Graph}] && Length[infraSpread @ r] == 3
  ],
  True,
  TestID -> "FindInfraSegment-explicit-count-enumerates"
]

(* ===== FindInfraSegment carries no Properties axis ===== *)

(* The symbol is the whole class: a rule narrowing the geodesic bundle is a local
   law at an infra-scale, hence a FindInfraGeodesic call. *)

VerificationTest[
  FindInfraSegment[GridGraph[{3, 3}], 1, 9, 1, Properties -> {"Minimizing"}],
  $Failed,
  {FindInfraSegment::badproperty},
  TestID -> "FindInfraSegment-Properties-axis-rejected"
]

VerificationTest[
  FindInfraSegment[GridGraph[{3, 3}], 1, 9, 1, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraSegment::badproperty},
  TestID -> "FindInfraSegment-badproperty-message"
]

VerificationTest[
  FindInfraSegment[GridGraph[{3, 3}], 1, 9, 1, Method -> "Unknown"],
  $Failed,
  {FindInfraSegment::badmethod},
  TestID -> "FindInfraSegment-badmethod-message"
]

(* The narrowed bundles now come from FindInfraGeodesic at scale Infinity, where
   "Minimizing" is the segment class and a selector refines it. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}],
        degSum = w |-> VertexDegree[GridGraph[{3, 3}], w[[-2]]] + VertexDegree[GridGraph[{3, 3}], w[[-1]]]},
    With[{paths = walkSeqs @ FindInfraGeodesic[g, 1, 9, Infinity, Infinity, All,
            Properties -> {"Minimizing", {"Minimal", degSum}}]},
      Length[paths] >= 1 &&
        AllTrue[paths, Length[#] - 1 == GraphDistance[g, 1, 9] &]
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Minimal-stays-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        degSum = w |-> VertexDegree[GridGraph[{4, 4}], w[[-2]]] + VertexDegree[GridGraph[{4, 4}], w[[-1]]]},
    BlockRandom[
      Length @ walkSeqs @ FindInfraGeodesic[g, 1, 16, Infinity, Infinity, All,
        Properties -> {"Minimizing", {"Minimal", degSum}},
        Method -> {"Exhaustive", "Pruning" -> 1}] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Minimal-pruning-beam-1"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}],
        degSum = w |-> VertexDegree[GridGraph[{3, 3}], w[[-2]]] + VertexDegree[GridGraph[{3, 3}], w[[-1]]]},
    Length @ walkSeqs @ FindInfraGeodesic[g, 1, 9, Infinity, Infinity, UpTo[2],
      Properties -> {"Minimizing", {"Minimal", degSum}}]
  ],
  _Integer?(# <= 2 &),
  SameTest -> MatchQ,
  TestID -> "FindInfraGeodesic-Minimal-UpTo-truncates"
]

(* "Greedy" Method on the segment class falls back to FindShortestPath. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ infraSpread @ FindInfraSegment[g, 1, 9, 1, Method -> "Greedy"]
  ],
  1,
  TestID -> "FindInfraSegment-Greedy-default-properties"
]


(* ===== FindInfraWalk (walk family) ===== *)

(* Properties is the class axis: the default is the simple paths, and
   "Generic" widens to the generic immersed walks. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    {w = First @ walkSeqs @ FindInfraWalk[g, 1, 9, Infinity, 1,
       Properties -> {"Simple"}]},
    InfraWalkQ[g, w] && DuplicateFreeQ[w]],
  True,
  TestID -> "FindInfraWalk-simple-properties-class"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[walkSeqs @ FindInfraWalk[g, 1, 9, UpTo[ 8 ], All], DuplicateFreeQ]],
  True,
  TestID -> "FindInfraWalk-default-simple"
]

(* "Greedy" is a supported Method (lazy DFS, one instance); an unrecognised
   Method string is still rejected. *)
VerificationTest[
  FindInfraWalk[GridGraph[{3, 3}], 1, 9, Infinity, 1, Method -> "Unknown"],
  $Failed,
  {FindInfraWalk::badmethod},
  TestID -> "FindInfraWalk-badmethod-message"
]

(* the default class has no repeats at any length. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{walks = walkSeqs @ FindInfraWalk[g, 1, 9, {4}, All]},
      Length[walks] >= 1 && AllTrue[walks, DuplicateFreeQ]
    ]
  ],
  True,
  TestID -> "FindInfraWalk-Simple-no-repeats"
]

(* The local geodesic rules live on FindInfraGeodesic: "Minimizing" at scale
   Infinity is the global geodesic from the first vertex. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ walkSeqs @ FindInfraGeodesic[g, 1, 9, Infinity, Infinity, All,
        Properties -> {"Simple", "Minimizing"}] ===
      Sort @ infraSpread @ FindInfraSegment[g, 1, 9, All]
  ],
  True,
  TestID -> "FindInfraGeodesic-Minimizing-scale-Infinity-equals-geodesics"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ walkSeqs @ FindInfraGeodesic[g, 1, 4, 2, Infinity, All,
        Properties -> {"Simple", "Minimizing"}]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraGeodesic-Minimizing-scale-2-cycle-geodesics"
]


VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ walkSeqs @ FindInfraGeodesic[g, 1, 4, 2, Infinity, All,
        Properties -> {"Simple", "Straightest"}]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraGeodesic-Straightest-scale-2-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ walkSeqs @ FindInfraGeodesic[g, 1, 16, 2, Infinity, All,
        Properties -> {"Simple", "Straightest"},
        Method -> {"Exhaustive", "Pruning" -> 1}] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Straightest-pruning-beam-1"
]

(* A selector composed with "Simple" still gives simple walks. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}],
        degSum = w |-> VertexDegree[GridGraph[{3, 3}], w[[-2]]] + VertexDegree[GridGraph[{3, 3}], w[[-1]]]},
    With[{walks = walkSeqs @ FindInfraGeodesic[g, 1, 9, 1, {4}, All,
            Properties -> {"Simple", {"Minimal", degSum}}]},
      AllTrue[walks, DuplicateFreeQ]
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Simple-Minimal-valid-walks"
]

(* ===== FindInfraLine ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    walkSequence @ FindInfraLine[g, 2, 4]
  ],
  {1, 2, 3, 4, 5},
  TestID -> "FindInfraLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ First @ infraSpread @ FindInfraLine[g, 2, 4]
  ],
  5,
  TestID -> "FindInfraLine-extends-to-full-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    walkSequence @ FindInfraLine[g, 1, 5]
  ],
  {1, 2, 3, 4, 5},
  TestID -> "FindInfraLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[SelectInfraWalk[g, infraSpread @ FindInfraLine[g, 5, 6, All], All, "From" -> "Center"], UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindInfraLine-with-SelectInfraWalk-Center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ infraSpread @ FindInfraLine[g, 2, 4, UpTo[5]] >= 1
  ],
  True,
  TestID -> "FindInfraLine-upto-soft"
]

(* the class is inextensibility, not length: the short line {1, 2, 3} is a line although the diameter is 3 *)
VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    infraSpread @ FindInfraLine[g, 1, 3, All]
  ],
  {{1, 2, 3}},
  TestID -> "FindInfraLine-keeps-short-inextensible-line"
]

(* C_6 through the edge 1-2: the ends {6, 5} and {3, 4} are each admissible alone, but (5, 4) is not jointly geodesic (d(5, 4) = 1), so the pool is the two extension DAGs plus the compatibility relation, three lines *)
VerificationTest[
  Sort @ infraSpread @ FindInfraLine[CycleGraph[6], 1, 2, All],
  Sort @ {{6, 1, 2, 3}, {1, 2, 3, 4}, {5, 6, 1, 2}},
  TestID -> "FindInfraLine-C6-compatibility"
]

(* one class under every Method: the greedy enumeration and the exhaustive pool agree, and every member is a line *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{exh = Sort @ infraSpread @ FindInfraLine[g, 1, 2, All, Method -> "Exhaustive"],
          grd = Sort @ infraSpread @ FindInfraLine[g, 1, 2, All, Method -> "Greedy"]},
      exh === grd && AllTrue[exh, InfraLineQ[g, #] &]
    ]
  ],
  True,
  TestID -> "FindInfraLine-class-invariant-GridGraph"
]

VerificationTest[
  With[{g = TorusGraph[{4, 5}]},
    With[{exh = Sort @ infraSpread @ FindInfraLine[g, 1, 2, All, Method -> "Exhaustive"],
          grd = Sort @ infraSpread @ FindInfraLine[g, 1, 2, All, Method -> "Greedy"]},
      exh === grd && AllTrue[exh, InfraLineQ[g, #] &]
    ]
  ],
  True,
  TestID -> "FindInfraLine-class-invariant-TorusGraph"
]

(* the pool carries the family by DP: its multiplicity and occupation are those of the enumerated lines *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pool = FindInfraLine[g, 1, 6, All]},
      infraNumReps @ pool === Length @ infraSpread @ pool &&
      Total @ toDensity[g, pool] === Total[Length /@ infraSpread @ pool]
    ]
  ],
  True,
  TestID -> "FindInfraLine-pool-DP-counts-match-enumeration"
]

(* the longest lines are a selection on the pool: SelectInfraWalk reads the atoms' lengths and keeps the pool form *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pool = FindInfraLine[g, 1, 2, All]},
      With[{longest = SelectInfraWalk[g, pool, All, "From" -> "MaxLength"]},
        MatchQ[longest, _Graph | {__Graph}] &&
        Sort @ infraSpread @ longest === Sort @ MaximalBy[infraSpread @ pool, Length]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraLine-longest-by-SelectInfraWalk-GridGraph"
]

VerificationTest[
  With[{g = TorusGraph[{4, 5}]},
    With[{pool = FindInfraLine[g, 1, 2, All]},
      Sort @ infraSpread @ SelectInfraWalk[g, pool, All, "From" -> "MaxLength"] ===
        Sort @ MaximalBy[infraSpread @ pool, Length]
    ]
  ],
  True,
  TestID -> "FindInfraLine-longest-by-SelectInfraWalk-TorusGraph"
]

(* ===== FindInfraShell ===== *)

(* Properties -> {} (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ FindInfraShell[g, 3, 2]
  ],
  {1, 5},
  TestID -> "FindInfraShell-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindInfraShell[g, 6, {1, 2}, All]},
      Length @ result == 1 &&
      AllTrue[First @ result, v |-> 1 <= GraphDistance[g, 6, v] <= 2]
    ]
  ],
  True,
  TestID -> "FindInfraShell-range-radius"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindInfraShell[g, 1, 2, All]
  ],
  1,
  TestID -> "FindInfraShell-default-single-result"
]

(* Properties -> {"Separating", "Connected"}: minimal connected separators. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}]},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]] &&
      AllTrue[shells, vs |-> ConnectedGraphQ[Subgraph[g, vs]]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Sep-Connected-within-range"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}]},
      AllTrue[shells, vs |-> AllTrue[shells,
        other |-> other === vs || ! (Length[other] < Length[vs] && SubsetQ[vs, other])
      ]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Sep-Connected-minimal"
]

(* Properties -> {"Separating"} alone (no connectedness requirement).
   Every returned vs is inside the level-set range; we don't re-test
   separation here because SeparatingSetQ is PackageScope and the
   admissibility predicate is enforced inside findShellCore. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating"}]},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Separating-only-no-connected-requirement"
]

(* The count-less call is one certified minimal shell -- the peel run to a leaf.
   Count-less is ONE instance, so the instance is that shell's vertex list. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shell = FindInfraShell[g, 6, {1, 2},
            Properties -> {"Separating", "Connected"}, Method -> "Greedy"]},
      MatchQ[shell, {__Integer}] && SeparatesQ[g, shell, 6, 16] ] ],
  True,
  TestID -> "FindInfraShell-Greedy-single-realisation"
]

(* "Greedy" is the LAZY peel, not a lossy one: it backtracks at each leaf, so a
   finite count is exact and All recovers the whole minimal class that
   "Exhaustive" enumerates. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], props = Properties -> { "Separating", "Connected" } },
    Sort[ Sort /@ FindInfraShell[ g, 6, { 1, 2 }, All, props, Method -> "Greedy" ] ] ===
      Sort[ Sort /@ FindInfraShell[ g, 6, { 1, 2 }, All, props, Method -> "Exhaustive" ] ] ],
  True,
  TestID -> "FindInfraShell-Greedy-All-agrees-with-Exhaustive"
]

(* RandomGreedy: the same peel drawn at random instead of in candidate order --
   deterministic Greedy unchanged, seeded reproducible, varies across seeds where
   the peel actually branches. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    FindInfraShell[ g, 6, { 1, 2 }, 1, Properties -> { "Separating", "Connected" }, Method -> "Greedy" ] ===
      FindInfraShell[ g, 6, { 1, 2 }, 1, Properties -> { "Separating", "Connected" }, Method -> "Greedy" ]
  ],
  True,
  TestID -> "FindInfraShell-Greedy-deterministic"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    BlockRandom[ FindInfraShell[ g, 6, { 1, 2 }, 1, Properties -> { "Separating", "Connected" }, Method -> "RandomGreedy" ], RandomSeeding -> 4 ] ===
      BlockRandom[ FindInfraShell[ g, 6, { 1, 2 }, 1, Properties -> { "Separating", "Connected" }, Method -> "RandomGreedy" ], RandomSeeding -> 4 ]
  ],
  True,
  TestID -> "FindInfraShell-RandomGreedy-seeded-reproducible"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ DeleteDuplicates @ Table[
      BlockRandom[ First @ FindInfraShell[ g, 6, { 1, 2 }, 1, Properties -> { "Separating", "Connected" }, Method -> "RandomGreedy" ], RandomSeeding -> s ],
      { s, 1, 10 } ]
  ],
  _Integer?( # > 1 & ),
  SameTest -> MatchQ,
  TestID -> "FindInfraShell-RandomGreedy-varies-across-seeds"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ] },
    FindInfraBisectingHyperplane[ g, 1, 36, 1, Properties -> { "Separating" }, Method -> "Greedy" ] ===
      FindInfraBisectingHyperplane[ g, 1, 36, 1, Properties -> { "Separating" }, Method -> "Greedy" ]
  ],
  True,
  TestID -> "FindInfraBisectingHyperplane-Greedy-deterministic"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ] },
    BlockRandom[ FindInfraBisectingHyperplane[ g, 1, 36, 1, Properties -> { "Separating" }, Method -> "RandomGreedy" ], RandomSeeding -> 4 ] ===
      BlockRandom[ FindInfraBisectingHyperplane[ g, 1, 36, 1, Properties -> { "Separating" }, Method -> "RandomGreedy" ], RandomSeeding -> 4 ]
  ],
  True,
  TestID -> "FindInfraBisectingHyperplane-RandomGreedy-seeded-reproducible"
]

(* Both greedy methods descend the geodesic interval, so every witness either
   produces is a genuine geodesic; the random one draws a different descent per
   seed while "Greedy" is reproducible without one. *)

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ] },
    { AllTrue[ infraSpread @ FindInfraSegment[ g, 1, 36, 4, Method -> "Greedy" ],
        InfraSegmentQ[ g, # ] & ],
      AllTrue[ Range[ 1, 6 ],
        s |-> InfraSegmentQ[ g,
          BlockRandom[ First @ FindInfraSegment[ g, 1, 36, 1, Method -> "RandomGreedy" ],
            RandomSeeding -> s ] ] ],
      Length @ DeleteDuplicates @ Table[
        BlockRandom[ First @ FindInfraSegment[ g, 1, 36, 1, Method -> "RandomGreedy" ],
          RandomSeeding -> s ],
        { s, 1, 10 } ] > 1 }
  ],
  { True, True, True },
  TestID -> "FindInfraSegment-greedy-methods-give-geodesics"
]


VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, All,
    Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}] >= 1,
  True,
  TestID -> "FindInfraShell-Pruning-bounded-runs"
]


VerificationTest[
  FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"NonExistent"}],
  $Failed,
  {FindInfraShell::badproperty},
  TestID -> "FindInfraShell-badproperty-message"
]

(* ===== FindInfraOsculatingShell ===== *)

(* On K5 with window {1, 2, 3} every other vertex is at distance 1 from
   each window-vertex, so vertices 4 and 5 are both osculating centers
   with radius 1; expect the two shells as a List of vertex lists. *)

VerificationTest[
  With[{result = FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, All]},
    MatchQ[result, {__List}] &&
    Length[result] === 2 &&
    Sort[Sort /@ result] === Sort[{Sort[{1, 2, 3, 5}], Sort[{1, 2, 3, 4}]}]
  ],
  True,
  TestID -> "FindInfraOsculatingShell-K5-two-osculating-centers"
]

(* Realisations are sorted by ascending radius, so the first one is the
   smallest-radius shell.  Both centers here have r = 1; tie-break by
   center index puts center 4 first. *)

VerificationTest[
  Sort @ First @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3],
  Sort[{1, 2, 3, 5}],
  TestID -> "FindInfraOsculatingShell-K5-default-smallest-radius"
]

(* PathGraph: window {3, 4, 5} has no integer vertex equidistant from
   all three.  count = All -> the empty class; exact count 1 -> $Failed. *)

VerificationTest[
  FindInfraOsculatingShell[PathGraph[Range[7]], Range[7], 4, 3, All],
  { },
  TestID -> "FindInfraOsculatingShell-PathGraph-no-centers-All"
]

VerificationTest[
  FindInfraOsculatingShell[PathGraph[Range[7]], Range[7], 4, 3, 1],
  $Failed,
  TestID -> "FindInfraOsculatingShell-PathGraph-no-centers-default-fails"
]


VerificationTest[
  FindInfraOsculatingShell[CompleteGraph[5], walkGraph @ {1, 2, 3}, 2, 3, All],
  FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, All],
  TestID -> "FindInfraOsculatingShell-walk-graph-equiv-bare-list"
]

(* a bundle of walk graphs: centers union across walks. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5],
    walkGraph /@ {{1, 2, 3}, {1, 4, 5}}, 2, 3, All],
  4,
  TestID -> "FindInfraOsculatingShell-multi-realisation-union"
]


VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, UpTo[1]],
  1,
  TestID -> "FindInfraOsculatingShell-UpTo-caps"
]


VerificationTest[
  FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, 3],
  $Failed,
  TestID -> "FindInfraOsculatingShell-count-exceeds-fails"
]

(* k = 1: trivial window {path[[i]]}; every vertex is an osculating
   center at its own distance to path[[i]], so we get one shell per
   vertex. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 1, All],
  VertexCount[CompleteGraph[5]],
  TestID -> "FindInfraOsculatingShell-k1-every-vertex"
]

(* Shells sorted by ascending radius: in K5 with k=1 the r=0 shell
   {path[[i]]} comes first (size 1), followed by the four r=1 shells
   (size 4 each). *)

VerificationTest[
  Length /@ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 1, All],
  {1, 4, 4, 4, 4},
  TestID -> "FindInfraOsculatingShell-sorted-by-radius"
]

(* ===== FindInfraCircle ===== *)

(* Default Properties -> {"Separating", "Shortest"}: tied-shortest separating cycles. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = infraSpread @ FindInfraCircle[g, 6, {1, 2}, All]},
      Length[circles] >= 1 && AllTrue[circles, Length[#] >= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-returns-cycles"
]

(* PetersenGraph[] is non-planar, so the circle pool refuses and the family
   comes from the cycle sweep instead -- reported by ::uncertified. *)
VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ infraSpread @ FindInfraCircle[g, 1, {1, 2}, All] >= 1
  ],
  True,
  {FindInfraCircle::uncertified},
  TestID -> "FindInfraCircle-all-cycles"
]

(* Default Properties include "Shortest": every returned cycle has the
   minimum admissible length, so all lengths are equal. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{lengths = Length /@ infraSpread @ FindInfraCircle[g, 6, {1, 2}, All]},
      Length[Union[lengths]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-default-tied-shortest"
]

(* the no-count canonical bundle lists a shortest cycle first. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shortest = First @ infraSpread @ FindInfraCircle[g, 6, {1, 2}],
          allLengths = Length /@ infraSpread @ FindInfraCircle[g, 6, {1, 2}, All]},
      Length[shortest] == Min[allLengths]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-default-returns-shortest"
]

(* Separating disconnects c from { d > rmax } and the shortest such cycle
   hugs the inner edge rmin, so widening the outer radius does not lengthen
   it: shortest({rmin, rmax+1}) == shortest({rmin, rmax}).  (The old
   mean-pinned separating test failed this -- {2,4} came out longer than
   {2,3}.) *)

VerificationTest[
  With[{g = GridGraph[{11, 11}]},
    With[{inner = Min[Length /@ infraSpread @ FindInfraCircle[g, First @ GraphCenter[g], {2, 3}]],
          wide  = Min[Length /@ infraSpread @ FindInfraCircle[g, First @ GraphCenter[g], {2, 4}]]},
      wide == inner
    ]
  ],
  True,
  TestID -> "FindInfraCircle-shortest-hugs-inner-edge"
]

(* Properties -> {"Separating"} (no "Shortest") accepts longer separating
   cycles too; cycles are length-ordered. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{lengths = Length /@ infraSpread @ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]},
      Length[lengths] >= 1 && lengths === Sort[lengths]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-drop-Shortest-length-ordered"
]

(* All returned cycles sit inside the level-set range. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = infraSpread @ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]},
      Length[circles] >= 1 &&
      AllTrue[circles, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-Separating-all-in-level-set"
]

(* Property "Connected" is not meaningful for cycles -> ::badproperty. *)

VerificationTest[
  FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"Connected"}],
  $Failed,
  {FindInfraCircle::badproperty},
  TestID -> "FindInfraCircle-badproperty-Connected"
]


VerificationTest[
  FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraCircle::badproperty},
  TestID -> "FindInfraCircle-badproperty-unknown"
]

VerificationTest[
  FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 1, Method -> "Bogus"],
  $Failed,
  {FindInfraCircle::badmethod},
  TestID -> "FindInfraCircle-badmethod"
]


(* ===== the circle pool: count All / count-less form ===== *)

(* On a certified band count = All gives the pool -- one arc-folded geodesic DAG
   per atom -- and every realisation of it is a genuine metric circle that
   separates the centre from outside the band. *)

VerificationTest[
  With[{g = GridGraph[{11, 11}], c = 61},
    With[{pool = FindInfraCircle[g, c, {2, 4}, All]},
      MatchQ[pool, {__Graph}] &&
      AllTrue[walkSequence /@ pool,
        cyc |-> DuplicateFreeQ[cyc] &&
          AllTrue[Partition[Append[cyc, First[cyc]], 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]] &&
      AllTrue[walkSequence /@ pool,
        cyc |-> AllTrue[
          SelectFirst[ConnectedComponents[VertexDelete[g, cyc]], MemberQ[#, c] &],
          GraphDistance[g, c, #] <= 4 &]]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-pool-realisations-are-separating-circles"
]

(* The pool's realisations are exactly the separating cycles of its own length:
   an independent single-length-class enumeration finds the same set. *)

VerificationTest[
  With[{g = GridGraph[{11, 11}], c = 61},
    With[{pool = FindInfraCircle[g, c, {2, 4}, All]},
      {level = Subgraph[g, Select[VertexList[g], 2 <= GraphDistance[g, c, #] <= 4 &]]},
      {byHand = Select[First /@ (List @@@ #) & /@ FindCycle[level, {First @ Union[EdgeCount /@ pool]}, All],
         cyc |-> With[{cc = SelectFirst[ConnectedComponents[VertexDelete[g, cyc]], MemberQ[#, c] &]},
           cc =!= Missing["NotFound"] && AllTrue[cc, GraphDistance[g, c, #] <= 4 &]]]},
      Sort[Sort /@ (walkSequence /@ pool)] === Sort[Sort /@ byHand]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-pool-equals-single-length-class-enumeration"
]

(* Every cycle is tied at the minimum circumference, so the class has one edge
   count, and a cycle graph has as many edges as vertices. *)

VerificationTest[
  With[{cycles = FindInfraCircle[GridGraph[{11, 11}], 61, {2, 4}, All]},
    Union[EdgeCount /@ cycles] === Union[Length /@ (walkSequence /@ cycles)]
  ],
  True,
  TestID -> "FindInfraCircle-pool-Length-is-the-common-circumference"
]

(* Property 3, the point of the pool: the marginals come off the atom DAGs by
   dynamic programming and agree with the enumerated ones. *)

VerificationTest[
  With[{g = GridGraph[{11, 11}]},
    {pool = FindInfraCircle[g, 61, {2, 4}, All]},
    {enumerated = geodesicCycleGraph /@ infraSpread @ pool},
    infraNumReps @ pool === Length[infraSpread @ pool] &&
    InfraDensity[g, pool] === InfraDensity[g, enumerated] &&
    KeySort[infraEdgeMultiset[g, pool]] === KeySort[infraEdgeMultiset[g, enumerated]]
  ],
  True,
  TestID -> "FindInfraCircle-pool-marginals-agree-with-enumeration"
]

(* A family too large to list: on a 25x25 grid band the count is a perfect
   fourth power, the circle factoring into four independent quadrant arcs.
   Enumeration cannot reach it; the DP answers in milliseconds. *)

VerificationTest[
  infraNumReps @ FindInfraCircle[GridGraph[{25, 25}], 313, {5, 9}, All],
  41^4,
  TestID -> "FindInfraCircle-pool-counts-an-unenumerable-family"
]

(* Lazy materialisation: a bounded count streams circles off the pool instead of
   enumerating the family. *)

VerificationTest[
  With[{g = GridGraph[{25, 25}]},
    With[{cycles = walkSequence /@ FindInfraCircle[g, 313, {5, 9}, UpTo[5]]},
      Length[cycles] === 5 &&
      AllTrue[cycles, Length[#] === 40 &] &&
      AllTrue[cycles,
        cyc |-> DuplicateFreeQ[cyc] &&
          AllTrue[Partition[Append[cyc, First[cyc]], 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-pool-bounded-count-is-lazy"
]

(* Off the certified class the answer is still exact -- it comes from the cycle
   sweep -- and ::uncertified says the pool was refused.  PetersenGraph[] is
   non-planar, so winding about the centre is undefined and the atom-invariance
   of separation fails. *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{cycles = walkSequence /@ FindInfraCircle[g, 1, {1, 2}, All]},
      cycles =!= {} &&
      AllTrue[cycles,
        cyc |-> DuplicateFreeQ[cyc] &&
          AllTrue[Partition[Append[cyc, First[cyc]], 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]]
    ]
  ],
  True,
  {FindInfraCircle::uncertified},
  TestID -> "FindInfraCircle-uncertified-falls-back-and-stays-exact"
]

(* An honestly empty family is not a refusal: a cycle graph's band carries no
   separating cycle, nothing is lost, and no message is emitted. *)

VerificationTest[
  infraSpread @ FindInfraCircle[CycleGraph[6], 1, {1, 2}, All],
  {},
  TestID -> "FindInfraCircle-empty-family-is-quiet"
]

(* The pool is no longer a carrier the caller can see: every Properties setting
   returns the same shape, a List of cycle graphs, and only the default one is
   answered lazily off the pool (that is the bounded-count test above).  Under
   the default the class is the tied-shortest one, so it is the smallest. *)

VerificationTest[
  With[{g = GridGraph[{11, 11}]},
    With[{classes = {
        FindInfraCircle[g, 61, {2, 4}, All],
        FindInfraCircle[g, 61, {2, 4}, All, Properties -> {"Shortest", "Separating"}],
        FindInfraCircle[g, 61, {2, 4}, All, Properties -> {"Separating"}],
        FindInfraCircle[g, 61, {2, 4}, All, Properties -> {}]}},
      { MatchQ[#, {__Graph}] & /@ classes,
        First @ classes === classes[[2]],
        Length @ First @ classes < Length @ classes[[3]] < Length @ Last @ classes } ] ],
  {{True, True, True, True}, True, True},
  TestID -> "FindInfraCircle-every-Properties-returns-cycle-graphs"
]

(* Under the default ({Separating, Shortest}) every returned cycle has the
   same length, so SelectInfraWalk's longest / shortest circumference
   selectors are trivially uniform.  Drop "Shortest" to get multiple lengths
   and exercise the selector. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraWalk[g, infraSpread @ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}], All, "From" -> "MaxLength", "Cyclic" -> True]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraWalk-MaxLength-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraWalk[g, infraSpread @ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}], All, "From" -> "MinLength", "Cyclic" -> True]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraWalk-MinLength"
]

(* ===== FindInfraParallel ===== *)

VerificationTest[
  infraSpread @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  {{5, 6, 7, 8}},
  TestID -> "FindInfraParallel-GridGraph-row-from-row"
]

VerificationTest[
  infraSpread @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  {{5, 6, 7, 8}},
  TestID -> "FindInfraParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  infraSpread @ FindInfraParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraParallel-self-on-line"
]

VerificationTest[
  FindInfraParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  { },
  TestID -> "FindInfraParallel-disconnected-empty"
]

VerificationTest[
  walkSequence /@ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  {{5, 6, 7, 8}},
  TestID -> "FindInfraParallel-strict-1"
]

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindInfraParallel-strict-fails-when-too-few"
]

(* All under "Exhaustive" hands back the pool itself, as FindInfraLine does; its
   realisations are the parallels -- here the middle row of the 5 x 5 grid, one
   carrier, so the lone bundle stands alone *)
VerificationTest[
  With[{pa = FindInfraParallel[GridGraph[{5, 5}], Range[5], 13, All]},
    {MatchQ[pa, _Graph | {__Graph}], infraSpread @ pa}],
  {True, {{11, 12, 13, 14, 15}}},
  TestID -> "FindInfraParallel-All-returns-the-pool"
]

VerificationTest[
  FindInfraParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  { },
  TestID -> "FindInfraParallel-CycleGraph-no-parallel"
]

VerificationTest[
  InfraParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    First @ infraSpread @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]],
  True,
  TestID -> "FindInfraParallel-output-passes-InfraParallelQ"
]


(* ===== EmbeddingClosest dispatch for InfraShell ===== *)

(* Sets ranked by directed Hausdorff distance to the Euclidean sphere of
   radius r centered at c. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    MatchQ[
      EmbeddingClosest[ g,
        List /@ Select[ VertexList[ g ], GraphDistance[ g, 6, # ] == 1 & ],
        { 6, 1 } ],
      { __List } ]
  ],
  True,
  TestID -> "EmbeddingClosest-set-family-returns-sets"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ EmbeddingClosest[ g, List /@ VertexList[ g ], { 6, 1 } ]
  ],
  16,
  TestID -> "EmbeddingClosest-set-family-all-vertices"
]


(* ===== EmbeddingClosest dispatch for InfraCircle (pre-existing) ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ EmbeddingClosest[ g,
      FindInfraCircle[ g, 6, { 1, 2 }, All, Properties -> { "Separating" } ],
      { 6, 1.5 } ] >= 1
  ],
  True,
  TestID -> "EmbeddingClosest-cycle-graphs-on-Separating-set"
]


(* ===== FindInfraPoint All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ (FindInfraPoint[ g, All ]) === VertexList[ g ]
  ],
  True,
  TestID -> "FindInfraPoint-All-returns-every-vertex"
]

VerificationTest[
  With[ { g = PetersenGraph[ ] },
    Length @ FindInfraPoint[ g, All ] == VertexCount[ g ]
  ],
  True,
  TestID -> "FindInfraPoint-All-length-equals-vertex-count"
]


(* ===== FindInfraLine unified Method axis ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ infraSpread @ FindInfraLine[ g, 1, 16, All, Method -> "Exhaustive" ] ===
      Sort @ infraSpread @ FindInfraLine[ g, 1, 16, All, Method -> Automatic ]
  ],
  True,
  TestID -> "FindInfraLine-Exhaustive-equals-Automatic"
]

(* ===== FindInfraLine / FindInfraParallel: "Greedy" vs "RandomGreedy" ===== *)

(* "Greedy" (candidate order) is fully deterministic -- no randomness is consumed. *)
VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ] },
    FindInfraLine[ g, 1, 2, 1, Method -> "Greedy" ] === FindInfraLine[ g, 1, 2, 1, Method -> "Greedy" ]
  ],
  True,
  TestID -> "FindInfraLine-Greedy-deterministic"
]

(* Regression guard: growing both sides of a Greedy line must re-derive the
   cross-distance against the OTHER side's live frontier, not the original
   anchor -- else the two arms can fail to concatenate into a geodesic. *)
VerificationTest[
  With[ { g = GridGraph[ { 7, 7 } ] },
    AllTrue[ Range[ 1, 48 ],
      p |-> InfraSegmentQ[ g, First @ FindInfraLine[ g, p, p + 1, 1, Method -> "Greedy" ] ] ]
  ],
  True,
  TestID -> "FindInfraLine-Greedy-BothSides-is-geodesic"
]

(* "RandomGreedy" (random branch order) is reproducible given an ambient
   SeedRandom -- no seed is threaded as a parameter. *)
VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ] },
    BlockRandom[ FindInfraLine[ g, 1, 2, 1, Method -> "RandomGreedy" ], RandomSeeding -> 11 ] ===
      BlockRandom[ FindInfraLine[ g, 1, 2, 1, Method -> "RandomGreedy" ], RandomSeeding -> 11 ]
  ],
  True,
  TestID -> "FindInfraLine-RandomGreedy-seeded-reproducible"
]

(* Different seeds explore different admissible chains where the graph branches. *)
VerificationTest[
  With[ { g = GridGraph[ { 8, 8 } ] },
    Length @ DeleteDuplicates @ Table[
      BlockRandom[ First @ FindInfraLine[ g, 20, 21, 1, Method -> "RandomGreedy" ], RandomSeeding -> s ],
      { s, 1, 10 } ]
  ],
  _Integer?( # > 1 & ),
  SameTest -> MatchQ,
  TestID -> "FindInfraLine-RandomGreedy-varies-across-seeds"
]

(* Every RandomGreedy realisation is still a genuine geodesic (BothSides fix applies here too). *)
VerificationTest[
  With[ { g = GridGraph[ { 7, 7 } ] },
    AllTrue[ Range[ 1, 5 ],
      s |-> InfraSegmentQ[ g,
        BlockRandom[ First @ FindInfraLine[ g, 25, 26, 1, Method -> "RandomGreedy" ], RandomSeeding -> s ] ] ]
  ],
  True,
  TestID -> "FindInfraLine-RandomGreedy-BothSides-is-geodesic"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ], line = First @ FindInfraLine[ GridGraph[ { 6, 6 } ], 1, 2, 1 ] },
    FindInfraParallel[ g, line, 20, 1, Method -> "Greedy" ] === FindInfraParallel[ g, line, 20, 1, Method -> "Greedy" ]
  ],
  True,
  TestID -> "FindInfraParallel-Greedy-deterministic"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ], line = First @ FindInfraLine[ GridGraph[ { 6, 6 } ], 1, 2, 1 ] },
    BlockRandom[ FindInfraParallel[ g, line, 20, 1, Method -> "RandomGreedy" ], RandomSeeding -> 3 ] ===
      BlockRandom[ FindInfraParallel[ g, line, 20, 1, Method -> "RandomGreedy" ], RandomSeeding -> 3 ]
  ],
  True,
  TestID -> "FindInfraParallel-RandomGreedy-seeded-reproducible"
]

(* ===== FindInfraLine[g, segment] overload (the extension pool at kspec Infinity, ExtendInfraSegment[g, seg]) ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ infraSpread @ FindInfraSegment[ g, 1, 6, All ] },
      With[ { lines = infraSpread @ FindInfraLine[ g, seg, All ] },
        ListQ[ lines ] && AllTrue[ lines,
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraLine-segment-contains-segment"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ infraSpread @ FindInfraSegment[ g, 1, 6, All ] },
      Sort @ infraSpread @ FindInfraLine[ g, seg, All ] ===
        Sort @ Select[ infraSpread @ FindInfraLine[ g, 1, 6, All ],
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
    ]
  ],
  True,
  TestID -> "FindInfraLine-segment-matches-endpoint-filtered"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    walkSequence /@ FindInfraLine[ g, { 2, 3 }, 1 ] === { { 1, 2, 3, 4, 5 } }
  ],
  True,
  TestID -> "FindInfraLine-segment-PathGraph-recovers-full-path"
]

VerificationTest[
  FindInfraLine[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 99 ],
  $Failed,
  TestID -> "FindInfraLine-segment-strict-undersupply-Failed"
]


(* ===== "Direction" option on FindInfraLine ===== *)

(* Forward-only extension of {3, 4} on PathGraph[7]: left end pinned at 3,
   right end extended to 7. *)
VerificationTest[
  walkSequence /@ FindInfraLine[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 1,
    "Direction" -> "Forward" ],
  { { 3, 4, 5, 6, 7 } },
  TestID -> "FindInfraLine-segment-Direction-Forward"
]

(* Backward-only extension of {3, 4} on PathGraph[7]: right end pinned at 4,
   left end extended to 1. *)
VerificationTest[
  walkSequence /@ FindInfraLine[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 1,
    "Direction" -> "Backward" ],
  { { 1, 2, 3, 4 } },
  TestID -> "FindInfraLine-segment-Direction-Backward"
]

VerificationTest[
  FindInfraLine[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 1 ] ===
    FindInfraLine[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 1,
      "Direction" -> "BothSides" ],
  True,
  TestID -> "FindInfraLine-segment-Direction-BothSides-default"
]

(* Forward on the two-point form: p1 fixed as line start. *)
VerificationTest[
  AllTrue[
    infraSpread @ FindInfraLine[ PathGraph[ Range[ 7 ] ], 3, 4, All,
      "Direction" -> "Forward" ],
    line |-> First[ line ] === 3 ],
  True,
  TestID -> "FindInfraLine-two-point-Direction-Forward-starts-at-p1"
]

VerificationTest[
  FindInfraLine[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 1,
    "Direction" -> "Sideways" ],
  $Failed,
  {FindInfraLine::baddirection},
  TestID -> "FindInfraLine-baddirection"
]


(* ===== ExtendInfraSegment: the extension pool on the distance matrix ===== *)

(* kspec Infinity is the line pool: FindInfraLine[g, seg] is ExtendInfraSegment[g, seg, Infinity], on a walk seed and on a geodesic-DAG seed *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindInfraSegment[ g, 1, 11, All ] },
      Sort @ infraSpread @ ExtendInfraSegment[ g, { 6, 7 }, Infinity, All ] ===
        Sort @ infraSpread @ FindInfraLine[ g, 6, 7, All ] &&
      Sort @ infraSpread @ ExtendInfraSegment[ g, seg, Infinity, All ] ===
        Sort @ infraSpread @ FindInfraLine[ g, seg, All ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-Infinity-is-FindInfraLine"
]

(* kspec 0 is the bundle itself: all six geodesics from 1 to 11 *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindInfraSegment[ g, 1, 11, All ] },
      Sort @ infraSpread @ ExtendInfraSegment[ g, seg, 0, All ] === Sort @ infraSpread @ seg
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-kspec-0-is-the-bundle"
]

(* every extension is a geodesic containing the seed, with at most k edges added on each side *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], seed = { 6, 7 }, k = 2 },
    AllTrue[ infraSpread @ ExtendInfraSegment[ g, seed, k, All ],
      w |-> InfraSegmentQ[ g, w ] &&
        With[ { pos = SequencePosition[ w, seed ] },
          pos =!= { } && pos[[ 1, 1 ]] - 1 <= k && Length[ w ] - pos[[ 1, 2 ]] <= k ] ]
  ],
  True,
  TestID -> "ExtendInfraSegment-extensions-are-geodesics-within-budget"
]

(* inextensible within the budget: a side still under budget has no neighbour prolonging the geodesic *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], seed = { 6, 7 }, k = 2 },
    AllTrue[ infraSpread @ ExtendInfraSegment[ g, seed, k, All ],
      w |-> With[ { pos = First @ SequencePosition[ w, seed ] },
        ( pos[[ 1 ]] - 1 == k ||
          NoneTrue[ AdjacencyList[ g, First @ w ], GraphDistance[ g, #, Last @ w ] == Length[ w ] & ] ) &&
        ( Length[ w ] - pos[[ 2 ]] == k ||
          NoneTrue[ AdjacencyList[ g, Last @ w ], GraphDistance[ g, First @ w, # ] == Length[ w ] & ] ) ] ]
  ],
  True,
  TestID -> "ExtendInfraSegment-maximal-within-budget"
]

(* agreement with the walk engine where the two ends never interact -- the interior grid edge and the path -- for a budget k, an exact {k} and a range *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    AllTrue[ { 1, 2, 3, { 2 }, { 1, 2 }, Infinity },
      k |-> Sort @ infraSpread @ ExtendInfraSegment[ g, { 6, 7 }, k, All ] ===
        Sort @ walkSeqs @ ExtendInfraGeodesic[ g, { 6, 7 }, Infinity, Replace[ k, n_Integer :> UpTo[ n ] ], All, Properties -> { "Minimizing" } ] ]
  ],
  True,
  TestID -> "ExtendInfraSegment-equals-walk-engine-GridGraph"
]

VerificationTest[
  AllTrue[ { 1, 2, 5, { 2 }, { 2, 5 }, Infinity },
    k |-> Sort @ infraSpread @ ExtendInfraSegment[ PathGraph[ Range[ 5 ] ], { 4, 5 }, k, All ] ===
      Sort @ walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 4, 5 }, Infinity, Replace[ k, n_Integer :> UpTo[ n ] ], All,
        Properties -> { "Minimizing" } ] ],
  True,
  TestID -> "ExtendInfraSegment-equals-walk-engine-PathGraph-asymmetric"
]

(* a geodesic-DAG seed extends as one object, the same set the walk engine gets by spreading over the six geodesics *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindInfraSegment[ g, 1, 11, All ] },
      Sort @ infraSpread @ ExtendInfraSegment[ g, seg, 1, All ] ===
        Sort @ walkSeqs @ ExtendInfraGeodesic[ g, seg, Infinity, UpTo[ 1 ], All, Properties -> { "Minimizing" } ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-bundle-seed-equals-walk-engine-spread"
]

(* where the ends interact the global observer sees more: on C_6 through the edge 1-2 a budget of 2 buys all three lines, the ones FindInfraLine finds; the two-sided walk engine, stepping both sides at once, reaches only {6, 1, 2, 3} *)
VerificationTest[
  Sort @ infraSpread @ ExtendInfraSegment[ CycleGraph[ 6 ], { 1, 2 }, 2, All ],
  Sort @ { { 6, 1, 2, 3 }, { 1, 2, 3, 4 }, { 5, 6, 1, 2 } },
  TestID -> "ExtendInfraSegment-C6-compatibility"
]

(* "Direction" as on FindInfraLine: only past the right end, only past the left end *)
VerificationTest[
  { infraSpread @ ExtendInfraSegment[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 2, All, "Direction" -> "Forward" ],
    infraSpread @ ExtendInfraSegment[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 2, All, "Direction" -> "Backward" ],
    infraSpread @ ExtendInfraSegment[ PathGraph[ Range[ 7 ] ], { 3, 4 }, Infinity, All, "Direction" -> "Forward" ] },
  { { { 3, 4, 5, 6 } }, { { 1, 2, 3, 4 } }, { { 3, 4, 5, 6, 7 } } },
  TestID -> "ExtendInfraSegment-Direction"
]

(* one class under every Method *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    SameQ @@ ( Sort @ infraSpread @ ExtendInfraSegment[ g, { 6, 7 }, 2, All, Method -> # ] & /@
      { "Exhaustive", "Greedy", "RandomGreedy" } )
  ],
  True,
  TestID -> "ExtendInfraSegment-class-invariant-under-Method"
]

(* the pool carries the family by DP -- one atom per admissible end pair, count, occupation, lengths and ends without enumeration *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { pool = ExtendInfraSegment[ g, { 6, 7 }, 2, All ] },
      MatchQ[ pool, { _Graph, __Graph } ] &&
      infraNumReps @ pool === Length @ infraSpread @ pool &&
      Total @ toDensity[ g, pool ] === Total[ Length /@ infraSpread @ pool ] &&
      Sort @ Union[ First /@ infraSpread @ pool ] === { 1, 9, 14 } &&
      Sort @ Union[ Last /@ infraSpread @ pool ] === { 4, 12, 15 }
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-pool-DP-counts-match-enumeration"
]

(* the longest extensions are a selection on the pool, and keep the pool form *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { pool = ExtendInfraSegment[ g, { 6, 7 }, Infinity, All ] },
      With[ { longest = SelectInfraWalk[ g, pool, All, "From" -> "MaxLength" ] },
        MatchQ[ longest, _Graph | { __Graph } ] &&
        Sort @ infraSpread @ longest === Sort @ MaximalBy[ infraSpread @ pool, Length ]
      ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-longest-by-SelectInfraWalk"
]

(* the count contract: count-less is one witness of the class, UpTo is soft, a strict n fails on under-supply *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { one = ExtendInfraSegment[ g, { 6, 7 }, 2 ], all = infraSpread @ ExtendInfraSegment[ g, { 6, 7 }, 2, All ] },
      Length @ infraSpread @ one == 1 && MemberQ[ all, First @ infraSpread @ one ] &&
      Length @ infraSpread @ ExtendInfraSegment[ g, { 6, 7 }, 2, UpTo[ 3 ] ] == 3 &&
      ExtendInfraSegment[ PathGraph[ Range[ 5 ] ], { 2, 3 }, Infinity, 99 ] === $Failed
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-count-contract"
]

(* a rule on the extension is a local law: it belongs to ExtendInfraGeodesic *)
VerificationTest[
  ExtendInfraSegment[ GridGraph[ { 4, 4 } ], { 6, 7 }, 2, All, Properties -> { "Simple" } ],
  $Failed,
  { ExtendInfraSegment::badproperty },
  TestID -> "ExtendInfraSegment-badproperty"
]

(* the 6-ary Tarski A4 form is untouched, with and without its count *)
VerificationTest[
  { ExtendInfraSegment[ PathGraph[ Range[ 5 ] ], 1, 2, 1, 2 ],
    ExtendInfraSegment[ PathGraph[ Range[ 5 ] ], 1, 2, 1, 2, All ] },
  { { 3 }, { 3 } },
  TestID -> "ExtendInfraSegment-A4-PathGraph"
]


(* FindInfraShell / FindInfraCircle: a bounded radius makes the answer depend only on
   the ball B(p, r + 1) / B(p, r + 2) around the centre.  The "Metric" and
   "Separating" recipes are graph-intrinsic; the "Embedding" recipe still
   uses the full graph for its spectral coordinates, so the local-vs-global
   cross-check is for "Metric" / "Separating". *)

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p = 45 },
    Sort[ Sort /@ FindInfraShell[ g, p, 2, All ] ] ===
      Sort[ Sort /@ FindInfraShell[ NeighborhoodGraph[ g, p, 3 ], p, 2, All ] ]
  ],
  True,
  TestID -> "FindInfraShell-locality-Metric"
]

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p = 45 },
    Sort[ Sort /@ infraSpread @ FindInfraCircle[ g, p, { 1, 2 }, All ] ] ===
      Sort[ Sort /@ infraSpread @ FindInfraCircle[ NeighborhoodGraph[ g, p, 4 ], p, { 1, 2 }, All ] ]
  ],
  True,
  TestID -> "FindInfraCircle-locality-Metric"
]


(* a capped line query returns members of the full family (the cap prunes the
   enumeration, never invents realisations) *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { family = infraSpread @ FindInfraLine[ g, 1, 9, All ] },
      MemberQ[ family, First @ infraSpread @ FindInfraLine[ g, 1, 9, 1 ] ] &&
        SubsetQ[ family, infraSpread @ FindInfraLine[ g, 1, 9, UpTo[ 3 ] ] ] ]
  ],
  True,
  TestID -> "FindInfraLine-cap-subset-of-family"
]

VerificationTest[
  Length @ infraSpread @ FindInfraLine[ GridGraph[ { 3, 3 } ], 1, 9, 3 ],
  3,
  TestID -> "FindInfraLine-strict-count-exact"
]

(* the diameter lines through opposite corners are the longest ones, a selection on the pool *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { longest = SelectInfraWalk[ g, FindInfraLine[ g, 1, 9, All ], All, "From" -> "MaxLength" ] },
      AllTrue[ infraSpread @ longest, Length[ # ] - 1 == GraphDiameter[ g ] & ] ]
  ],
  True,
  TestID -> "FindInfraLine-diameter-lines-by-MaxLength"
]

EndTestSection[]
