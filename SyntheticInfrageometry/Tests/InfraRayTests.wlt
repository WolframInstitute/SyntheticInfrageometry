BeginTestSection["InfraRay"]

realisations = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;
walkSequence = WolframInstitute`SyntheticInfrageometry`PackageScope`walkSequence;
infraNumReps = WolframInstitute`SyntheticInfrageometry`PackageScope`infraNumReps;

(* ===== FindInfraRay: the class ===== *)

(* A ray from o through v is a geodesic from o containing v that cannot be prolonged past its
   last vertex.  The whole class, against a brute-force FindPath enumeration. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    {d = GraphDistanceMatrix[g]},
    {rays = Catenate @ Table[
        Select[FindPath[g, 1, e, {d[[1, e]]}, All],
          path |-> MemberQ[path, 2] && NoneTrue[AdjacencyList[g, e], d[[1, #]] == d[[1, e]] + 1 &]],
        {e, DeleteCases[VertexList[g], 1]}]},
    Sort @ realisations @ FindInfraRay[g, 1, 2, All] === Sort @ rays],
  True,
  TestID -> "FindInfraRay-class-equals-brute-force-grid-neighbour"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    {d = GraphDistanceMatrix[g]},
    {rays = Catenate @ Table[
        Select[FindPath[g, 1, e, {d[[1, e]]}, All],
          path |-> MemberQ[path, 6] && NoneTrue[AdjacencyList[g, e], d[[1, #]] == d[[1, e]] + 1 &]],
        {e, DeleteCases[VertexList[g], 1]}]},
    Sort @ realisations @ FindInfraRay[g, 1, 6, All] === Sort @ rays],
  True,
  TestID -> "FindInfraRay-class-equals-brute-force-grid-diagonal"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    {d = GraphDistanceMatrix[g]},
    {rays = Catenate @ Table[
        Select[FindPath[g, 1, e, {d[[1, e]]}, All],
          path |-> MemberQ[path, 2] && NoneTrue[AdjacencyList[g, e], d[[1, #]] == d[[1, e]] + 1 &]],
        {e, DeleteCases[VertexList[g], 1]}]},
    Sort @ realisations @ FindInfraRay[g, 1, 2, All] === Sort @ rays],
  True,
  TestID -> "FindInfraRay-class-equals-brute-force-petersen"
]

(* The FindInfraRayMaximality spread table: every (origin, neighbour) pair on the seven
   fixtures, every emitted ray satisfies InfraRayQ.  At 0.13.17 six of the seven failed. *)
VerificationTest[
  AllTrue[
    {PathGraph[Range[7]], CycleGraph[7], GridGraph[{3, 3}], GridGraph[{4, 4}], GridGraph[{5, 5}],
     PetersenGraph[], HypercubeGraph[3]},
    g |-> AllTrue[
      Join[List @@@ EdgeList[g], Reverse /@ List @@@ EdgeList[g]],
      pair |-> InfraRayQ[g, FindInfraRay[g, pair[[1]], pair[[2]], All]]]],
  True,
  TestID -> "FindInfraRay-InfraRayQ-agree-on-spread-table"
]

(* One class under every Method. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    SameQ @@ (Sort @ realisations @ FindInfraRay[g, 6, 7, All, Method -> #] & /@
      {"Exhaustive", "Greedy", "RandomGreedy"})],
  True,
  TestID -> "FindInfraRay-class-invariant-under-Method"
]

(* Rays from o through o are all the rays from o: the pool is the spray of o. *)
VerificationTest[
  Sort @ realisations @ FindInfraRay[CycleGraph[6], 1, 1, All],
  Sort @ PencilDirections[CycleGraph[6], 1],
  TestID -> "FindInfraRay-origin-as-direction-gives-the-pencil"
]

(* ===== FindInfraRay: the shapes ===== *)

(* the count-less call is one ray, a directed path graph on the substrate starting at the origin *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]}, {r = FindInfraRay[g, 6, 7]},
    GraphQ[r] && InfraRayQ[g, r] &&
      First @ walkSequence @ r === 6 && MemberQ[VertexList @ r, 7]],
  True,
  TestID -> "FindInfraRay-count-less-is-one-ray"
]

(* All is the pool, one DAG with source o whose o -> sink paths are the rays; its DP count is the family size *)
VerificationTest[
  With[{r = FindInfraRay[GridGraph[{4, 4}], 6, 7, All]},
    GraphQ[r] && AcyclicGraphQ[r] &&
      infraNumReps[r] === Length @ realisations @ r &&
      Pick[VertexList @ r, VertexInDegree @ r, 0] === {6}],
  True,
  TestID -> "FindInfraRay-pool-is-a-DAG-from-the-origin"
]

VerificationTest[
  realisations @ FindInfraRay[PathGraph[Range[7]], 4, 7, All],
  {{4, 5, 6, 7}},
  TestID -> "FindInfraRay-PathGraph-toward-end"
]

VerificationTest[
  Sort @ realisations @ FindInfraRay[CycleGraph[6], 1, 4, All],
  {{1, 2, 3, 4}, {1, 6, 5, 4}},
  TestID -> "FindInfraRay-CycleGraph6-antipode-two-realisations"
]

VerificationTest[
  With[{r = FindInfraRay[GridGraph[{3, 3}], 1, 9, 1]},
    MatchQ[r, {_Graph}] && First @ walkSequence @ First @ r === 1 &&
      Last @ walkSequence @ First @ r === 9],
  True,
  TestID -> "FindInfraRay-GridGraph-strict-1"
]

VerificationTest[
  FindInfraRay[CycleGraph[6], 1, 4, 5],
  $Failed,
  TestID -> "FindInfraRay-strict-shortfall"
]

VerificationTest[
  Length @ FindInfraRay[CycleGraph[6], 1, 4, UpTo[10]],
  2,
  TestID -> "FindInfraRay-UpTo-soft"
]

(* From 2 the ray through 4 runs on to 5: d(2, 5) == 3 == d(2, 4) + 1, and 6 is no farther.  Two origins give two pools. *)
VerificationTest[
  Sort @ realisations @ FindInfraRay[CycleGraph[6], <| 1 -> 1, 2 -> 1 |>, 4, All],
  {{1, 2, 3, 4}, {1, 6, 5, 4}, {2, 3, 4, 5}},
  TestID -> "FindInfraRay-multi-anchor-spreads-over-origins"
]

VerificationTest[
  FindInfraRay[CycleGraph[6], 1, 4, Method -> "Nonsense"],
  $Failed,
  {FindInfraRay::badmethod},
  TestID -> "FindInfraRay-badmethod"
]

(* ===== PencilDirections, PencilCardinality: the pencil is the set of rays ===== *)

VerificationTest[
  Sort @ PencilDirections[PathGraph[Range[7]], 4],
  {{4, 3, 2, 1}, {4, 5, 6, 7}},
  TestID -> "PencilDirections-PathGraph-two-rays"
]

VerificationTest[
  PencilCardinality[PathGraph[Range[7]], 4],
  2,
  TestID -> "PencilCardinality-PathGraph"
]

VerificationTest[
  PencilCardinality[CycleGraph[6], 1],
  2,
  TestID -> "PencilCardinality-CycleGraph6"
]

VerificationTest[
  PencilCardinality[CycleGraph[7], 1],
  2,
  TestID -> "PencilCardinality-CycleGraph7"
]

(* Centre of the 3x3 grid: two rays through each of the four neighbours, ending at the corners. *)
VerificationTest[
  PencilCardinality[GridGraph[{3, 3}], 5],
  8,
  TestID -> "PencilCardinality-GridGraph3x3-centre"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[PencilDirections[g, 5], First[#] === 5 && InfraRayQ[g, #] &]],
  True,
  TestID -> "PencilDirections-are-rays-from-O"
]

VerificationTest[
  Count[PencilDirections[CycleGraph[6], 1], ray_ /; MemberQ[ray, 4]],
  2,
  TestID -> "PencilDirections-CycleGraph6-antipode-two-rays"
]

(* DP count against enumeration. *)
VerificationTest[
  Length @ PencilDirections[GridGraph[{3, 3}], 5],
  PencilCardinality[GridGraph[{3, 3}], 5],
  TestID -> "PencilDirections-Cardinality-agree-grid"
]

VerificationTest[
  Length @ PencilDirections[HypercubeGraph[3], 1],
  PencilCardinality[HypercubeGraph[3], 1],
  TestID -> "PencilDirections-Cardinality-agree-hypercube"
]

EndTestSection[]
