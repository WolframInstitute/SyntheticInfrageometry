BeginTestSection["EuclideanPostulates"]

(* ===== FindInfraPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g]},
      Head[pt] === InfraPoint && MemberQ[VertexList[g], pt["Vertex"]]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindInfraPoint[g, 3]},
      Length @ pts == 3 && DuplicateFreeQ[(#["Vertex"] & /@ pts)] && SubsetQ[VertexList[g], (#["Vertex"] & /@ pts)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], (#["Vertex"] & /@ FindInfraPoint[g, 1, "From" -> "Center"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], (#["Vertex"] & /@ FindInfraPoint[g, 1, "From" -> "Periphery"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-periphery"
]

(* {"Center", 0} runs zero iterations -- identical pool to plain "Center". *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort[#["Vertex"] & /@ FindInfraPoint[g, All, "From" -> {"Center", 0}]] ===
      Sort[#["Vertex"] & /@ FindInfraPoint[g, All, "From" -> "Center"]]
  ],
  True,
  TestID -> "FindInfraPoint-iterated-center-cap0-equals-center"
]

(* On a graph with a dominating center the orbit is already a fixed point, so the
   iterated (uncapped) pool is the center of that fixed point -- a subset of the graph. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    SubsetQ[VertexList[g], #["Vertex"] & /@ FindInfraPoint[g, All, "From" -> {"Center", Infinity}]]
  ],
  True,
  TestID -> "FindInfraPoint-iterated-center-stabilizes-in-graph"
]

(* A disconnected input is already a shattered region: the pool is its whole vertex set. *)
VerificationTest[
  With[{g = GraphDisjointUnion[PathGraph[{1, 2, 3}], PathGraph[{4, 5, 6}]]},
    Sort[#["Vertex"] & /@ FindInfraPoint[g, All, "From" -> {"Center", Infinity}]] === VertexList[g]
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

(* the count-less form validates on the same vocabulary *)
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
         7, 1 -> 2, {2, 3, 4}, InfraPoint[9], InfraSet[{2, 5, 7}],
         InfraMesoPoint[<|3 -> 1, 4 -> 1|>]},
      $Failed]
  ],
  True,
  TestID -> "FindInfraPoint-From-vocabulary-not-refused"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#["Vertex"] & /@ FindInfraPoint[g, 2, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindInfraPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    With[{vs = #["Vertex"] & /@ FindInfraPoint[g, 4, "Distance" -> "Max"]},
      Min[GraphDistance[g, #[[1]], #[[2]]] & /@ Subsets[vs, {2}]] == 5
    ]
  ],
  True,
  TestID -> "FindInfraPoint-Max-maximizes-minimum-gap"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    With[{spread = #["Vertex"] & /@ FindInfraPoint[g, 4, "Distance" -> "Spread"],
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
      Length @ pt == 1 && SubsetQ[{2, 3, 4}, (#["Vertex"] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#["Vertex"] & /@ FindInfraPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4])},
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
    Length @ pts == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], (#["Vertex"] & /@ pts)]
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
      Length @ pt == 1 && GraphDistance[g, 3, First[pt]["Vertex"]] == 2
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#["Vertex"] & /@ FindInfraPoint[g, UpTo[20], "From" -> 1 -> {2, 3}])},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = (#["Vertex"] & /@ FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"])},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-max-distance"
]

VerificationTest[
  MatchQ[ FindInfraPoint[ PetersenGraph[], 3 ], { InfraPoint[_] .. } ],
  True,
  TestID -> "FindInfraPoint-returns-list-of-unary-InfraPoint"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#["Vertex"] & /@ FindInfraPoint[g, UpTo[VertexCount[g]],
      "From" -> InfraSet[{1, 16}] -> 3])},
      AllTrue[pts, GraphDistance[g, 1, #] == 3 && GraphDistance[g, 16, #] == 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multi-anchor-intersection"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ (#["Vertex"] & /@ FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> InfraSet[{2, 5, 7}]])
  ],
  {2, 5, 7},
  TestID -> "FindInfraPoint-multi-anchor-pool-no-distance"
]

(* ===== FindInfraSegment ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindInfraSegment[g, 1, 5, All]["Realizations"]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = FindInfraSegment[g, 1, 3, All]["Paths"]},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindInfraSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindInfraSegment[g, 1, 9, All]["Realizations"]},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (SelectInfraWalk[g, FindInfraSegment[g, 1, 9, All]["Realizations"],All, "From" -> "Center", "Metric" -> "Frechet"])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraWalk-Center-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosest[g, FindInfraSegment[g, 1, 9, All]["Realizations"], {1, 9}]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EmbeddingClosest"
]

VerificationTest[
  FindInfraSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  InfraSegment[{}],
  TestID -> "FindInfraSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (SelectInfraWalk[g, FindInfraSegment[g, 1, 9, All]["Realizations"],All, "From" -> "Center", "Metric" -> "Hausdorff"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraWalk-Center-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (SelectInfraWalk[g, FindInfraSegment[g, 1, 9, All]["Realizations"],All, "From" -> "Periphery"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraWalk-Periphery"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosest[g, {1, 9}] @ SelectInfraWalk[g, All, "From" -> "Center"] @
        (FindInfraSegment[g, 1, 9, All]["Realizations"])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindInfraSegment[g, 1, 9, UpTo[2]]["Realizations"]},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-upto-soft-cap"
]

(* ===== FindInfraSegment geodesic-DAG default form ===== *)

(* Bare / All with empty Properties over vertex endpoints returns the compact
   geodesic interval DAG (InfraSegment[_Graph]); its invariants are read off the
   DAG -- multiplicity (family size), the common geodesic length, and the interval
   vertex set. *)
VerificationTest[
  With[{s = FindInfraSegment[GridGraph[{3, 3}], 1, 9, All]},
    {Head[s], Head[First[s]], s["Multiplicity"], s["Length"], Sort[s["Vertices"]]}
  ],
  {InfraSegment, Graph, 6, 4, Range[9]},
  TestID -> "FindInfraSegment-default-is-geodesic-dag"
]

(* the DAG-form occupation measure equals the measure of the enumerated family *)
VerificationTest[
  With[{s = FindInfraSegment[GridGraph[{3, 3}], 1, 9, All]},
    KeySort[s["Measure"]] === KeySort[InfraMeasure[InfraSegment[s["Paths"]]]]
  ],
  True,
  TestID -> "FindInfraSegment-dag-measure-matches-enumeration"
]

(* Realizations bridges back to the explicit bare path list, capped by the calling triple *)
VerificationTest[
  With[{s = FindInfraSegment[GridGraph[{3, 3}], 1, 9, All]},
    {Length[s["Realizations"]], Length[s["Realizations", UpTo[3]]]}
  ],
  {6, 3},
  TestID -> "FindInfraSegment-dag-realizations-bridge"
]

(* an explicit finite count returns one wrapper carrying exactly n realisations *)
VerificationTest[
  With[{r = FindInfraSegment[GridGraph[{3, 3}], 1, 9, 3]},
    MatchQ[r, InfraSegment[{_, _, _}]] && Length[r["Realizations"]] == 3
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
    With[{paths = FindInfraGeodesic[g, 1, 9, Infinity, Infinity, All,
            Properties -> {"Minimizing", {"Minimal", degSum}}]["Realizations"]},
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
      Length @ FindInfraGeodesic[g, 1, 16, Infinity, Infinity, All,
        Properties -> {"Minimizing", {"Minimal", degSum}},
        Method -> {"Exhaustive", "Pruning" -> 1}]["Realizations"] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Minimal-pruning-beam-1"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}],
        degSum = w |-> VertexDegree[GridGraph[{3, 3}], w[[-2]]] + VertexDegree[GridGraph[{3, 3}], w[[-1]]]},
    Length @ FindInfraGeodesic[g, 1, 9, Infinity, Infinity, UpTo[2],
      Properties -> {"Minimizing", {"Minimal", degSum}}]["Realizations"]
  ],
  _Integer?(# <= 2 &),
  SameTest -> MatchQ,
  TestID -> "FindInfraGeodesic-Minimal-UpTo-truncates"
]

(* "Greedy" Method on the segment class falls back to FindShortestPath. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, 1, Method -> "Greedy"]["Realizations"]
  ],
  1,
  TestID -> "FindInfraSegment-Greedy-default-properties"
]


(* ===== FindInfraWalk Properties (path family) ===== *)

VerificationTest[
  FindInfraWalk[GridGraph[{3, 3}], 1, 9, Infinity, 1, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraWalk::badproperty},
  TestID -> "FindInfraWalk-badproperty-message"
]

(* "Greedy" is a supported Method (lazy DFS, one instance); an unrecognised
   Method string is still rejected. *)
VerificationTest[
  FindInfraWalk[GridGraph[{3, 3}], 1, 9, Infinity, 1, Method -> "Unknown"],
  $Failed,
  {FindInfraWalk::badmethod},
  TestID -> "FindInfraWalk-badmethod-message"
]

(* "Simple" Property: opt-in simplicity. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{walks = FindInfraWalk[g, 1, 9, {4}, All, Properties -> {"Simple"}]["Realizations"]},
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
    Sort @ FindInfraGeodesic[g, 1, 9, Infinity, Infinity, All,
        Properties -> {"Simple", "Minimizing"}]["Realizations"] ===
      Sort @ FindInfraSegment[g, 1, 9, All]["Realizations"]
  ],
  True,
  TestID -> "FindInfraGeodesic-Minimizing-scale-Infinity-equals-geodesics"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindInfraGeodesic[g, 1, 4, 2, Infinity, All,
        Properties -> {"Simple", "Minimizing"}]["Realizations"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraGeodesic-Minimizing-scale-2-cycle-geodesics"
]

(* "Straightest": pull-apart walks. *)

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindInfraGeodesic[g, 1, 4, 2, Infinity, All,
        Properties -> {"Simple", "Straightest"}]["Realizations"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraGeodesic-Straightest-scale-2-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ FindInfraGeodesic[g, 1, 16, 2, Infinity, All,
        Properties -> {"Simple", "Straightest"},
        Method -> {"Exhaustive", "Pruning" -> 1}]["Realizations"] == 1,
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
    With[{walks = FindInfraGeodesic[g, 1, 9, 1, {4}, All,
            Properties -> {"Simple", {"Minimal", degSum}}]["Realizations"]},
      AllTrue[walks, DuplicateFreeQ]
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Simple-Minimal-valid-walks"
]

(* ===== FindInfraLine ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindInfraLine[g, 2, 4]
  ],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ First @ FindInfraLine[g, 2, 4]["Realizations"]
  ],
  5,
  TestID -> "FindInfraLine-extends-to-full-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindInfraLine[g, 1, 5]
  ],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[SelectInfraWalk[g, FindInfraLine[g, 5, 6, All]["Realizations"], All, "From" -> "Center"], UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindInfraLine-with-SelectInfraWalk-Center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindInfraLine[g, 2, 4, UpTo[5]]["Realizations"] >= 1
  ],
  True,
  TestID -> "FindInfraLine-upto-soft"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 3, All, "Maximality" -> "Extension"]
  ],
  InfraLine[{{1, 2, 3}}],
  TestID -> "FindInfraLine-Maximality-Extension-keeps-short-line"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 3, All, "Maximality" -> "Diameter"]
  ],
  InfraLine[{}],
  TestID -> "FindInfraLine-Maximality-Diameter-drops-short-line"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 5, All, "Maximality" -> "Diameter"]
  ],
  InfraLine[{{1, 2, 4, 5}}],
  TestID -> "FindInfraLine-Maximality-Diameter-keeps-diameter-line"
]

(* ===== FindInfraShell ===== *)

(* Properties -> {} (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ First @ FindInfraShell[g, 3, 2]["Realizations"]
  ],
  {1, 5},
  TestID -> "FindInfraShell-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindInfraShell[g, 6, {1, 2}, All]},
      Length @ result == 1 &&
      AllTrue[First @ First @ First @ result, v |-> 1 <= GraphDistance[g, 6, v] <= 2]
    ]
  ],
  True,
  TestID -> "FindInfraShell-range-radius"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindInfraShell[g, 1, 2, All]["Realizations"]
  ],
  1,
  TestID -> "FindInfraShell-default-single-result"
]

(* Properties -> {"Separating", "Connected"}: minimal connected separators. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}]["Realizations"]},
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
    With[{shells = FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}]["Realizations"]},
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
    With[{shells = FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating"}]["Realizations"]},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Separating-only-no-connected-requirement"
]

(* The count-less call is one certified minimal shell -- the peel run to a leaf. *)

VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2},
    Properties -> {"Separating", "Connected"}, Method -> "Greedy"]["Realizations"],
  1,
  TestID -> "FindInfraShell-Greedy-single-realisation"
]

(* "Greedy" is the LAZY peel, not a lossy one: it backtracks at each leaf, so a
   finite count is exact and All recovers the whole minimal class that
   "Exhaustive" enumerates. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], props = Properties -> { "Separating", "Connected" } },
    Sort[ Sort /@ FindInfraShell[ g, 6, { 1, 2 }, All, props, Method -> "Greedy" ][ "Realizations" ] ] ===
      Sort[ Sort /@ FindInfraShell[ g, 6, { 1, 2 }, All, props, Method -> "Exhaustive" ][ "Realizations" ] ] ],
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
      BlockRandom[ FindInfraShell[ g, 6, { 1, 2 }, 1, Properties -> { "Separating", "Connected" }, Method -> "RandomGreedy" ][ "First" ], RandomSeeding -> s ],
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
    { AllTrue[ FindInfraSegment[ g, 1, 36, 4, Method -> "Greedy" ][ "Realizations" ],
        InfraSegmentQ[ g, # ] & ],
      AllTrue[ Range[ 1, 6 ],
        s |-> InfraSegmentQ[ g,
          BlockRandom[ FindInfraSegment[ g, 1, 36, 1, Method -> "RandomGreedy" ][ "First" ],
            RandomSeeding -> s ] ] ],
      Length @ DeleteDuplicates @ Table[
        BlockRandom[ FindInfraSegment[ g, 1, 36, 1, Method -> "RandomGreedy" ][ "First" ],
          RandomSeeding -> s ],
        { s, 1, 10 } ] > 1 }
  ],
  { True, True, True },
  TestID -> "FindInfraSegment-greedy-methods-give-geodesics"
]

(* Method -> {"Exhaustive", "Pruning" -> n} respects branching cap. *)

VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, All,
    Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}]["Realizations"] >= 1,
  True,
  TestID -> "FindInfraShell-Pruning-bounded-runs"
]

(* Unknown property name raises ::badproperty. *)

VerificationTest[
  FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"NonExistent"}],
  $Failed,
  {FindInfraShell::badproperty},
  TestID -> "FindInfraShell-badproperty-message"
]

(* ===== FindInfraOsculatingShell ===== *)

(* On K5 with window {1, 2, 3} every other vertex is at distance 1 from
   each window-vertex, so vertices 4 and 5 are both osculating centers
   with radius 1; expect one shell wrapper with two realisations. *)

VerificationTest[
  With[{result = FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, All]},
    Head[result] === InfraShell &&
    Length[result["Realizations"]] === 2 &&
    Sort[Sort /@ result["Realizations"]] === Sort[{Sort[{1, 2, 3, 5}], Sort[{1, 2, 3, 4}]}]
  ],
  True,
  TestID -> "FindInfraOsculatingShell-K5-two-osculating-centers"
]

(* Realisations are sorted by ascending radius, so the first one is the
   smallest-radius shell.  Both centers here have r = 1; tie-break by
   center index puts center 4 first. *)

VerificationTest[
  Sort @ First @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3]["Realizations"],
  Sort[{1, 2, 3, 5}],
  TestID -> "FindInfraOsculatingShell-K5-default-smallest-radius"
]

(* PathGraph: window {3, 4, 5} has no integer vertex equidistant from
   all three.  count = All -> InfraShell[{}]; exact count 1 -> $Failed. *)

VerificationTest[
  FindInfraOsculatingShell[PathGraph[Range[7]], Range[7], 4, 3, All],
  InfraShell[{}],
  TestID -> "FindInfraOsculatingShell-PathGraph-no-centers-All"
]

VerificationTest[
  FindInfraOsculatingShell[PathGraph[Range[7]], Range[7], 4, 3, 1],
  $Failed,
  TestID -> "FindInfraOsculatingShell-PathGraph-no-centers-default-fails"
]

(* InfraWalk[{walk}] equivalent to bare list. *)

VerificationTest[
  FindInfraOsculatingShell[CompleteGraph[5], InfraWalk[{{1, 2, 3}}], 2, 3, All],
  FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, All],
  TestID -> "FindInfraOsculatingShell-InfraWalk-equiv-bare-list"
]

(* Multi-realisation InfraWalk: centers union across walks. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5],
    InfraWalk[{{1, 2, 3}, {1, 4, 5}}], 2, 3, All]["Realizations"],
  4,
  TestID -> "FindInfraOsculatingShell-multi-realisation-union"
]

(* UpTo[n] caps below the available count. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, UpTo[1]]["Realizations"],
  1,
  TestID -> "FindInfraOsculatingShell-UpTo-caps"
]

(* Strict count > available returns $Failed. *)

VerificationTest[
  FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, 3],
  $Failed,
  TestID -> "FindInfraOsculatingShell-count-exceeds-fails"
]

(* k = 1: trivial window {path[[i]]}; every vertex is an osculating
   center at its own distance to path[[i]], so we get one shell per
   vertex. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 1, All]["Realizations"],
  VertexCount[CompleteGraph[5]],
  TestID -> "FindInfraOsculatingShell-k1-every-vertex"
]

(* Shells sorted by ascending radius: in K5 with k=1 the r=0 shell
   {path[[i]]} comes first (size 1), followed by the four r=1 shells
   (size 4 each). *)

VerificationTest[
  Length /@ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 1, All]["Realizations"],
  {1, 4, 4, 4, 4},
  TestID -> "FindInfraOsculatingShell-sorted-by-radius"
]

(* ===== FindInfraCircle ===== *)

(* Default Properties -> {"Separating", "Shortest"}: tied-shortest separating cycles. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = FindInfraCircle[g, 6, {1, 2}, All]["Realizations"]},
      Length[circles] >= 1 && AllTrue[circles, Length[#] >= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindInfraCircle[g, 1, {1, 2}, All]["Realizations"] >= 1
  ],
  True,
  TestID -> "FindInfraCircle-all-cycles"
]

(* Default Properties include "Shortest": every returned cycle has the
   minimum admissible length, so all lengths are equal. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{lengths = Length /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All])},
      Length[Union[lengths]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-default-tied-shortest"
]

(* the no-count canonical bundle lists a shortest cycle first. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shortest = First @ FindInfraCircle[g, 6, {1, 2}]["Realizations"],
          allLengths = Length /@ FindInfraCircle[g, 6, {1, 2}, All]["Realizations"]},
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
    With[{inner = First @ First @ First @ FindInfraCircle[g, First @ GraphCenter[g], {2, 3}],
          wide  = First @ First @ First @ FindInfraCircle[g, First @ GraphCenter[g], {2, 4}]},
      Length[wide] == Length[inner]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-shortest-hugs-inner-edge"
]

(* Properties -> {"Separating"} (no "Shortest") accepts longer separating
   cycles too; cycles are length-ordered. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{lengths = Length /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}])},
      Length[lengths] >= 1 && lengths === Sort[lengths]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-drop-Shortest-length-ordered"
]

(* All returned cycles sit inside the level-set range. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = #[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]},
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

(* Any unknown property raises ::badproperty. *)

VerificationTest[
  FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraCircle::badproperty},
  TestID -> "FindInfraCircle-badproperty-unknown"
]

(* Under the default ({Separating, Shortest}) every returned cycle has the
   same length, so SelectInfraWalk's longest / shortest circumference
   selectors are trivially uniform.  Drop "Shortest" to get multiple lengths
   and exercise the selector. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraWalk[g, FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]["Realizations"], All, "From" -> "MaxLength", "Cyclic" -> True]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraWalk-MaxLength-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraWalk[g, FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]["Realizations"], All, "From" -> "MinLength", "Cyclic" -> True]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraWalk-MinLength"
]

(* ===== FindInfraParallel ===== *)

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-GridGraph-row-from-row"
]

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  FindInfraParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraParallel-self-on-line"
]

VerificationTest[
  FindInfraParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  InfraLine[{}],
  TestID -> "FindInfraParallel-disconnected-empty"
]

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-strict-1"
]

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindInfraParallel-strict-fails-when-too-few"
]

VerificationTest[
  FindInfraParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  InfraLine[{}],
  TestID -> "FindInfraParallel-CycleGraph-no-parallel"
]

VerificationTest[
  InfraParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    First @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]["Realizations"]],
  True,
  TestID -> "FindInfraParallel-output-passes-InfraParallelQ"
]


(* ===== EmbeddingClosest dispatch for InfraShell ===== *)

(* Sets ranked by directed Hausdorff distance to the Euclidean sphere of
   radius r centered at c. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ EmbeddingClosest[ g,
      InfraShell[ List /@ Select[ VertexList[ g ], GraphDistance[ g, 6, # ] == 1 & ] ],
      { 6, 1 } ]
  ],
  InfraShell,
  TestID -> "EmbeddingClosest-InfraShell-preserves-wrapper"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ First @ EmbeddingClosest[ g,
      InfraShell[ List /@ VertexList[ g ] ], { 6, 1 } ]
  ],
  16,
  TestID -> "EmbeddingClosest-InfraShell-pool-all-vertices"
]


(* ===== EmbeddingClosest dispatch for InfraCircle (pre-existing) ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ First @ EmbeddingClosest[ g,
      FindInfraCircle[ g, 6, { 1, 2 }, All, Properties -> { "Separating" } ],
      { 6, 1.5 } ] >= 1
  ],
  True,
  TestID -> "EmbeddingClosest-InfraCircle-on-Separating-set"
]


(* ===== FindInfraPoint All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ (#["Vertex"] & /@ FindInfraPoint[ g, All ]) === VertexList[ g ]
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
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> "Exhaustive" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> Automatic ])
  ],
  True,
  TestID -> "FindInfraLine-Exhaustive-equals-Automatic"
]

(* ===== FindInfraLine / FindInfraParallel: "Greedy" vs "RandomGreedy" ===== *)

(* "Greedy" (pick = First) is fully deterministic -- no randomness is consumed. *)
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
      p |-> InfraSegmentQ[ g, FindInfraLine[ g, p, p + 1, 1, Method -> "Greedy" ][ "First" ] ] ]
  ],
  True,
  TestID -> "FindInfraLine-Greedy-BothSides-is-geodesic"
]

(* "RandomGreedy" (pick = RandomChoice) is reproducible given an ambient
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
      BlockRandom[ FindInfraLine[ g, 20, 21, 1, Method -> "RandomGreedy" ][ "First" ], RandomSeeding -> s ],
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
        BlockRandom[ FindInfraLine[ g, 25, 26, 1, Method -> "RandomGreedy" ][ "First" ], RandomSeeding -> s ] ] ]
  ],
  True,
  TestID -> "FindInfraLine-RandomGreedy-BothSides-is-geodesic"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ], line = FindInfraLine[ GridGraph[ { 6, 6 } ], 1, 2, 1 ][ "First" ] },
    FindInfraParallel[ g, line, 20, 1, Method -> "Greedy" ] === FindInfraParallel[ g, line, 20, 1, Method -> "Greedy" ]
  ],
  True,
  TestID -> "FindInfraParallel-Greedy-deterministic"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ], line = FindInfraLine[ GridGraph[ { 6, 6 } ], 1, 2, 1 ][ "First" ] },
    BlockRandom[ FindInfraParallel[ g, line, 20, 1, Method -> "RandomGreedy" ], RandomSeeding -> 3 ] ===
      BlockRandom[ FindInfraParallel[ g, line, 20, 1, Method -> "RandomGreedy" ], RandomSeeding -> 3 ]
  ],
  True,
  TestID -> "FindInfraParallel-RandomGreedy-seeded-reproducible"
]

(* ===== FindInfraLine[g, segment] overload (replaces 2-arg ExtendInfraSegment) ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ FindInfraSegment[ g, 1, 6, All ][ "Realizations" ] },
      With[ { lines = FindInfraLine[ g, seg, All ][ "Realizations" ] },
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
    With[ { seg = First @ FindInfraSegment[ g, 1, 6, All ][ "Realizations" ] },
      Sort @ FindInfraLine[ g, seg, All ][ "Realizations" ] ===
        Sort @ Select[ FindInfraLine[ g, 1, 6, All ][ "Realizations" ],
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
    ]
  ],
  True,
  TestID -> "FindInfraLine-segment-matches-endpoint-filtered"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    FindInfraLine[ g, { 2, 3 }, 1 ] === InfraLine[ { { 1, 2, 3, 4, 5 } } ]
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
  FindInfraLine[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 1,
    "Direction" -> "Forward" ],
  InfraLine[ { { 3, 4, 5, 6, 7 } } ],
  TestID -> "FindInfraLine-segment-Direction-Forward"
]

(* Backward-only extension of {3, 4} on PathGraph[7]: right end pinned at 4,
   left end extended to 1. *)
VerificationTest[
  FindInfraLine[ PathGraph[ Range[ 7 ] ], { 3, 4 }, 1,
    "Direction" -> "Backward" ],
  InfraLine[ { { 1, 2, 3, 4 } } ],
  TestID -> "FindInfraLine-segment-Direction-Backward"
]

(* Default BothSides matches the explicit value. *)
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
    FindInfraLine[ PathGraph[ Range[ 7 ] ], 3, 4, All,
      "Direction" -> "Forward" ][ "Realizations" ],
    line |-> First[ line ] === 3 ],
  True,
  TestID -> "FindInfraLine-two-point-Direction-Forward-starts-at-p1"
]

(* Unknown direction -> $Failed with ::baddirection *)
VerificationTest[
  FindInfraLine[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 1,
    "Direction" -> "Sideways" ],
  $Failed,
  {FindInfraLine::baddirection},
  TestID -> "FindInfraLine-baddirection"
]


(* ===== Tarski A4 (5-arg ExtendInfraSegment, the only surviving form) ===== *)

(* Tested in TarskiGeometryTests via the synthetic-extension axiom dashboard;
   here we just sanity-check the calling-triple shape. *)

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    ExtendInfraSegment[ g, 1, 2, 1, 2, All ]
  ],
  { InfraPoint[3] },
  TestID -> "ExtendInfraSegment-A4-PathGraph"
]




(* FindInfraShell / FindInfraCircle: a bounded radius makes the answer depend only on
   the ball B(p, r + 1) / B(p, r + 2) around the centre.  The "Metric" and
   "Separating" recipes are graph-intrinsic; the "Embedding" recipe still
   uses the full graph for its spectral coordinates, so the local-vs-global
   cross-check is for "Metric" / "Separating". *)

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p = 45 },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraShell[ g, p, 2, All ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraShell[ NeighborhoodGraph[ g, p, 3 ], p, 2, All ])
  ],
  True,
  TestID -> "FindInfraShell-locality-Metric"
]

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p = 45 },
    Sort[ Sort /@ FindInfraCircle[ g, p, { 1, 2 }, All ][ "Realizations" ] ] ===
      Sort[ Sort /@ FindInfraCircle[ NeighborhoodGraph[ g, p, 4 ], p, { 1, 2 }, All ][ "Realizations" ] ]
  ],
  True,
  TestID -> "FindInfraCircle-locality-Metric"
]


(* a capped line query returns members of the full family (the cap prunes the
   enumeration, never invents realisations) *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { family = FindInfraLine[ g, 1, 9, All ][ "Realizations" ] },
      MemberQ[ family, FindInfraLine[ g, 1, 9, 1 ][ "First" ] ] &&
        SubsetQ[ family, FindInfraLine[ g, 1, 9, UpTo[ 3 ] ][ "Realizations" ] ] ]
  ],
  True,
  TestID -> "FindInfraLine-cap-subset-of-family"
]

(* strict count on an abundant family *)
VerificationTest[
  Length @ FindInfraLine[ GridGraph[ { 3, 3 } ], 1, 9, 3 ][ "Realizations" ],
  3,
  TestID -> "FindInfraLine-strict-count-exact"
]

(* "Diameter" maximality post-filters the whole family, cap or not *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { line = FindInfraLine[ g, 1, 9, 1, "Maximality" -> "Diameter" ][ "First" ] },
      Length[ line ] - 1 == GraphDiameter[ g ] ]
  ],
  True,
  TestID -> "FindInfraLine-Diameter-maximality-with-count"
]

EndTestSection[]
