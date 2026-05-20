BeginTestSection["EuclideanPostulates"]

(* ===== FindInfraPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g]},
      Length @ pt == 1 && SubsetQ[VertexList[g], (#[[ 1, 1 ]] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindInfraPoint[g, 3]},
      Length @ pts == 3 && DuplicateFreeQ[(#[[ 1, 1 ]] & /@ pts)] && SubsetQ[VertexList[g], (#[[ 1, 1 ]] & /@ pts)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 1, "From" -> "Center"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 1, "From" -> "Periphery"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-periphery"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 2, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindInfraPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g, 1, "From" -> {2, 3, 4}]},
      Length @ pt == 1 && SubsetQ[{2, 3, 4}, (#[[ 1, 1 ]] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4])},
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
    Length @ pts == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], (#[[ 1, 1 ]] & /@ pts)]
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
      Length @ pt == 1 && GraphDistance[g, 3, First @ First @ First @ pt] == 2
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[20], "From" -> 1 -> {2, 3}])},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"])},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-max-distance"
]

VerificationTest[
  MatchQ[ FindInfraPoint[ PetersenGraph[] ], { InfraPoint[ { _ } ] .. } ],
  True,
  TestID -> "FindInfraPoint-returns-list-of-unary-InfraPoint"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[VertexCount[g]],
      "From" -> InfraPoint[{1, 16}] -> 3])},
      AllTrue[pts, GraphDistance[g, 1, #] == 3 && GraphDistance[g, 16, #] == 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multi-anchor-intersection"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> InfraPoint[{2, 5, 7}]])
  ],
  {2, 5, 7},
  TestID -> "FindInfraPoint-multi-anchor-pool-no-distance"
]

(* ===== FindInfraSegment ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindInfraSegment[g, 1, 5]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3])},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindInfraSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All])},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraSegment[g, 1, 9, All], All, "From" -> "Center", "Metric" -> "Frechet"])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraPath-Center-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ EmbeddingClosest[g, FindInfraSegment[g, 1, 9, All], {1, 9}])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EmbeddingClosest"
]

VerificationTest[
  InfraSegment @ FindInfraSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  InfraSegment[{}],
  TestID -> "FindInfraSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraSegment[g, 1, 9, All], All, "From" -> "Center", "Metric" -> "Hausdorff"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraPath-Center-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraSegment[g, 1, 9, All], All, "From" -> "Periphery"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraPath-Periphery"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosest[g, {1, 9}] @ SelectInfraPath[g, All, "From" -> "Center"] @
        (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, UpTo[2]])},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-upto-soft-cap"
]

(* ===== FindInfraSegment Properties (geodesic family) ===== *)

(* Geodesic-ness is implicit; "ShortestPath" Property is rejected. *)

VerificationTest[
  FindInfraSegment[GridGraph[{3, 3}], 1, 9, 1, Properties -> {"ShortestPath"}],
  $Failed,
  {FindInfraSegment::badproperty},
  TestID -> "FindInfraSegment-ShortestPath-Property-rejected"
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

(* {"EdgeMin", f}: among geodesics, keep those MinimalBy f[v, w] at each step.
   degSum is a synthetic edge function (sum of vertex degrees on the edge). *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], degSum = {a, b} |-> VertexDegree[GridGraph[{3, 3}], a] + VertexDegree[GridGraph[{3, 3}], b]},
    With[{paths = #[[1, 1]] & /@ FindInfraSegment[g, 1, 9, All, Properties -> {{"EdgeMin", degSum}}]},
      Length[paths] >= 1 &&
        AllTrue[paths, Length[#] - 1 == GraphDistance[g, 1, 9] &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EdgeMin-stays-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], degSum = {a, b} |-> VertexDegree[GridGraph[{3, 3}], a] + VertexDegree[GridGraph[{3, 3}], b]},
    SubsetQ[
      Sort @ (#[[1, 1]] & /@ FindInfraSegment[g, 1, 9, All]),
      Sort @ (#[[1, 1]] & /@ FindInfraSegment[g, 1, 9, All, Properties -> {{"EdgeMin", degSum}}])
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EdgeMin-is-subset-of-geodesics"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], degSum = {a, b} |-> VertexDegree[GridGraph[{4, 4}], a] + VertexDegree[GridGraph[{4, 4}], b]},
    BlockRandom[
      Length @ FindInfraSegment[g, 1, 16, All,
        Properties -> {{"EdgeMin", degSum}},
        Method -> {"Exhaustive", "Pruning" -> 1}] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EdgeMin-pruning-beam-1"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], degSum = {a, b} |-> VertexDegree[GridGraph[{3, 3}], a] + VertexDegree[GridGraph[{3, 3}], b]},
    Length @ FindInfraSegment[g, 1, 9, UpTo[2], Properties -> {{"EdgeMin", degSum}}]
  ],
  _Integer?(# <= 2 &),
  SameTest -> MatchQ,
  TestID -> "FindInfraSegment-EdgeMin-UpTo-truncates"
]

(* {"LongestPath", "Window" -> k}: among geodesics, MaximalBy distance-tuple
   to the last k vertices.  Result is a subset of the full geodesic bundle. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SubsetQ[
      Sort @ (#[[1, 1]] & /@ FindInfraSegment[g, 1, 9, All]),
      Sort @ (#[[1, 1]] & /@ FindInfraSegment[g, 1, 9, All, Properties -> {{"LongestPath", "Window" -> 2}}])
    ]
  ],
  True,
  TestID -> "FindInfraSegment-LongestPath-Window2-subset-of-geodesics"
]

(* "Greedy" Method on default Properties = {} falls back to FindShortestPath. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, 1, Method -> "Greedy"]
  ],
  1,
  TestID -> "FindInfraSegment-Greedy-default-properties"
]


(* ===== FindInfraPath Properties (path family) ===== *)

VerificationTest[
  FindInfraPath[GridGraph[{3, 3}], 1, 9, Infinity, 1, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraPath::badproperty},
  TestID -> "FindInfraPath-badproperty-message"
]

VerificationTest[
  FindInfraPath[GridGraph[{3, 3}], 1, 9, Infinity, 1, Method -> "Greedy"],
  $Failed,
  {FindInfraPath::badmethod},
  TestID -> "FindInfraPath-badmethod-Greedy-rejected"
]

(* "Simple" Property: opt-in simplicity. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{walks = #[[1, 1]] & /@ FindInfraPath[g, 1, 9, {4}, All, Properties -> {"Simple"}]},
      Length[walks] >= 1 && AllTrue[walks, DuplicateFreeQ]
    ]
  ],
  True,
  TestID -> "FindInfraPath-Simple-no-repeats"
]

(* {"ShortestPath", "Window" -> k}: K-local geodesic walks.  Window = Infinity
   forces global geodesic from path[[1]]. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[1, 1]] & /@ FindInfraPath[g, 1, 9, Infinity, All,
        Properties -> {"Simple", {"ShortestPath", "Window" -> Infinity}}]) ===
      Sort @ (#[[1, 1]] & /@ FindInfraSegment[g, 1, 9, All])
  ],
  True,
  TestID -> "FindInfraPath-ShortestPath-WindowInf-equals-geodesics"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[1, 1]] & /@ FindInfraPath[g, 1, 4, Infinity, All,
        Properties -> {"Simple", {"ShortestPath", "Window" -> 2}}])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraPath-ShortestPath-Window2-cycle-geodesics"
]

(* {"LongestPath", "Window" -> k}: pull-apart walks. *)

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[1, 1]] & /@ FindInfraPath[g, 1, 4, Infinity, All,
        Properties -> {"Simple", {"LongestPath", "Window" -> 2}}])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraPath-LongestPath-Window2-cycle-symmetric"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    Sort @ (#[[1, 1]] & /@ FindInfraPath[g, 1, 3, Infinity, All,
        Properties -> {"Simple", {"LongestPath", "Window" -> 2}}])
  ],
  Sort[{{1, 2, 3}, {1, 4, 3}}],
  TestID -> "FindInfraPath-LongestPath-Window2-strict-between"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ FindInfraPath[g, 1, 16, Infinity, All,
        Properties -> {"Simple", {"LongestPath", "Window" -> 2}},
        Method -> {"Exhaustive", "Pruning" -> 1}] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraPath-LongestPath-pruning-beam-1"
]

(* {"EdgeMin", f} composed with "Simple" gives valid simple walks. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], degSum = {a, b} |-> VertexDegree[GridGraph[{3, 3}], a] + VertexDegree[GridGraph[{3, 3}], b]},
    With[{walks = #[[1, 1]] & /@ FindInfraPath[g, 1, 9, {4}, All,
            Properties -> {"Simple", {"EdgeMin", degSum}}]},
      AllTrue[walks, DuplicateFreeQ]
    ]
  ],
  True,
  TestID -> "FindInfraPath-Simple-EdgeMin-valid-walks"
]

(* ===== FindInfraLine ===== *)

VerificationTest[
  InfraLine @ With[{g = PathGraph[Range[5]]},
    FindInfraLine[g, 2, 4]
  ],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ First @ First @ First @ FindInfraLine[g, 2, 4]
  ],
  5,
  TestID -> "FindInfraLine-extends-to-full-path"
]

VerificationTest[
  InfraLine @ With[{g = PathGraph[Range[5]]},
    FindInfraLine[g, 1, 5]
  ],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[(#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraLine[g, 5, 6, All], All, "From" -> "Center"]), UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindInfraLine-with-SelectInfraPath-Center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindInfraLine[g, 2, 4, UpTo[5]] >= 1
  ],
  True,
  TestID -> "FindInfraLine-upto-soft"
]

VerificationTest[
  InfraLine @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 3, All, "Maximality" -> "Extension"]
  ],
  InfraLine[{{1, 2, 3}}],
  TestID -> "FindInfraLine-Maximality-Extension-keeps-short-line"
]

VerificationTest[
  InfraLine @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 3, All, "Maximality" -> "Diameter"]
  ],
  InfraLine[{}],
  TestID -> "FindInfraLine-Maximality-Diameter-drops-short-line"
]

VerificationTest[
  InfraLine @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 5, All, "Maximality" -> "Diameter"]
  ],
  InfraLine[{{1, 2, 4, 5}}],
  TestID -> "FindInfraLine-Maximality-Diameter-keeps-diameter-line"
]

(* ===== FindInfraShell ===== *)

(* Properties -> {} (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ First @ First @ First @ FindInfraShell[g, 3, 2]
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
    Length @ FindInfraShell[g, 1, 2, All]
  ],
  1,
  TestID -> "FindInfraShell-default-single-result"
]

(* Properties -> {"Separating", "Connected"}: minimal connected separators. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = (#[[ 1, 1 ]] & /@ FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}])},
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
    With[{shells = (#[[ 1, 1 ]] & /@ FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}])},
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
    With[{shells = (#[[ 1, 1 ]] & /@ FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating"}])},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Separating-only-no-connected-requirement"
]

(* Method -> "Greedy" returns a single realisation. *)

VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, All,
    Properties -> {"Separating", "Connected"}, Method -> "Greedy"],
  1,
  TestID -> "FindInfraShell-Greedy-single-realisation"
]

VerificationTest[
  FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, 2,
    Properties -> {"Separating", "Connected"}, Method -> "Greedy"],
  $Failed,
  TestID -> "FindInfraShell-Greedy-count-gt-1-fails"
]

(* Method -> {"Exhaustive", "Pruning" -> n} respects branching cap. *)

VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, All,
    Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}] >= 1,
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
   with radius 1; expect two unary shells. *)

VerificationTest[
  With[{result = FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, All]},
    Length[result] === 2 &&
    AllTrue[result, MatchQ[#, InfraShell[{_List}]] &] &&
    Sort[Sort /@ (#[[ 1, 1 ]] & /@ result)] === Sort[{Sort[{1, 2, 3, 5}], Sort[{1, 2, 3, 4}]}]
  ],
  True,
  TestID -> "FindInfraOsculatingShell-K5-two-osculating-centers"
]

(* Default count = 1 picks the smallest-radius shell.  Both centers
   here have r = 1; tie-break by center index picks center 4. *)

VerificationTest[
  Sort @ First @ First @ First @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3],
  Sort[{1, 2, 3, 5}],
  TestID -> "FindInfraOsculatingShell-K5-default-smallest-radius"
]

(* PathGraph: window {3, 4, 5} has no integer vertex equidistant from
   all three.  count = All -> {}; count = 1 -> $Failed. *)

VerificationTest[
  FindInfraOsculatingShell[PathGraph[Range[7]], Range[7], 4, 3, All],
  {},
  TestID -> "FindInfraOsculatingShell-PathGraph-no-centers-All"
]

VerificationTest[
  FindInfraOsculatingShell[PathGraph[Range[7]], Range[7], 4, 3],
  $Failed,
  TestID -> "FindInfraOsculatingShell-PathGraph-no-centers-default-fails"
]

(* InfraPath[{walk}] equivalent to bare list. *)

VerificationTest[
  FindInfraOsculatingShell[CompleteGraph[5], InfraPath[{{1, 2, 3}}], 2, 3, All],
  FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, All],
  TestID -> "FindInfraOsculatingShell-InfraPath-equiv-bare-list"
]

(* Multi-realisation InfraPath: centers union across walks. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5],
    InfraPath[{{1, 2, 3}, {1, 4, 5}}], 2, 3, All],
  4,
  TestID -> "FindInfraOsculatingShell-multi-realisation-union"
]

(* UpTo[n] caps below the available count. *)

VerificationTest[
  Length @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 3, UpTo[1]],
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
  Length @ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 1, All],
  VertexCount[CompleteGraph[5]],
  TestID -> "FindInfraOsculatingShell-k1-every-vertex"
]

(* Shells sorted by ascending radius: in K5 with k=1 the r=0 shell
   {path[[i]]} comes first (size 1), followed by the four r=1 shells
   (size 4 each). *)

VerificationTest[
  Length[#[[ 1, 1 ]]] & /@ FindInfraOsculatingShell[CompleteGraph[5], {1, 2, 3}, 2, 1, All],
  {1, 4, 4, 4, 4},
  TestID -> "FindInfraOsculatingShell-sorted-by-radius"
]

(* ===== FindInfraCircle ===== *)

(* Default Properties -> {"Separating", "Shortest"}: tied-shortest separating cycles. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = #[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All]},
      Length[circles] >= 1 && AllTrue[circles, Length[#] >= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindInfraCircle[g, 1, {1, 2}, All] >= 1
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

(* count = 1 default returns the shortest cycle. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shortest = First @ First @ First @ FindInfraCircle[g, 6, {1, 2}],
          allLengths = Length /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All])},
      Length[shortest] == Min[allLengths]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-default-returns-shortest"
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
   same length, so SelectInfraCycle's longest / shortest circumference
   selectors are trivially uniform.  Drop "Shortest" to get multiple lengths
   and exercise the selector. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraCycle[g, (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]), All, "From" -> "LongestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraCycle-LongestCircumference-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraCycle[g, (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]), All, "From" -> "ShortestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraCycle-ShortestCircumference"
]

(* ===== FindInfraParallel ===== *)

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-GridGraph-row-from-row"
]

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  InfraLine @ FindInfraParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraParallel-self-on-line"
]

VerificationTest[
  InfraLine @ FindInfraParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  InfraLine[{}],
  TestID -> "FindInfraParallel-disconnected-empty"
]

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-strict-1"
]

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindInfraParallel-strict-fails-when-too-few"
]

VerificationTest[
  InfraLine @ FindInfraParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  InfraLine[{}],
  TestID -> "FindInfraParallel-CycleGraph-no-parallel"
]

VerificationTest[
  InfraParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    First @ First @ First @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]],
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
      InfraCircle[ #[[ 1, 1 ]] & /@ FindInfraCircle[ g, 6, { 1, 2 }, All, Properties -> { "Separating" } ] ],
      { 6, 1.5 } ] >= 1
  ],
  True,
  TestID -> "EmbeddingClosest-InfraCircle-on-Separating-set"
]


(* ===== FindInfraPoint All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraPoint[ g, All ]) === VertexList[ g ]
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
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> "ShortestPath" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> Automatic ])
  ],
  True,
  TestID -> "FindInfraLine-Shortest-equals-Automatic"

(* ===== FindInfraLine[g, segment] overload (replaces 2-arg ExtendInfraSegment) ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      With[ { lines = (#[[ 1, 1 ]] & /@ FindInfraLine[ g, seg, All ]) },
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
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, seg, All ]) ===
        Sort @ Select[ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 6, All ]),
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
    ]
  ],
  True,
  TestID -> "FindInfraLine-segment-matches-endpoint-filtered"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    InfraLine @ FindInfraLine[ g, { 2, 3 }, 1 ] === InfraLine[ { { 1, 2, 3, 4, 5 } } ]
  ],
  True,
  TestID -> "FindInfraLine-segment-PathGraph-recovers-full-path"
]

VerificationTest[
  FindInfraLine[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 99 ],
  $Failed,
  TestID -> "FindInfraLine-segment-strict-undersupply-Failed"
]


(* ===== Tarski A4 (5-arg ExtendInfraSegment, the only surviving form) ===== *)

(* Tested in TarskiGeometryTests via the synthetic-extension axiom dashboard;
   here we just sanity-check the calling-triple shape. *)

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    InfraPoint @ ExtendInfraSegment[ g, 1, 2, 1, 2, All ]
  ],
  InfraPoint[ { { 3 } } ],
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
    Sort[ Sort /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[ g, p, { 1, 2 }, All ]) ] ===
      Sort[ Sort /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[ NeighborhoodGraph[ g, p, 4 ], p, { 1, 2 }, All ]) ]
  ],
  True,
  TestID -> "FindInfraCircle-locality-Metric"
]

EndTestSection[]
