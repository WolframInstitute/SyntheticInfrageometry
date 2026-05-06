Package["WolframInstitute`SyntheticInfrageometry`"]

(* Usage messages: one sentence per signature; options listed by name only.
   Full method/sub-option trees and worked examples live in the guides
   and tutorials, not here. See CLAUDE.md "Usage messages" for the rules. *)

(* ===================== EuclideanPostulates ===================== *)

FindPoint::usage = "FindPoint[graph] returns one vertex as InfraPoint[{v}]. FindPoint[graph, n] returns exactly n vertices wrapped as InfraPoint[{v1, ..., vn}] or $Failed; UpTo[n] returns up to n; All returns every vertex. Options: \"From\", \"Distance\", \"MaxCliques\".";
FindSegment::usage = "FindSegment[graph, p1, p2] returns InfraSegment[{path}] for the shortest path. FindSegment[graph, p1, p2, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Endpoints accept InfraPoint[{...}] for multi-anchor spread. Options: Method (\"Shortest\" (default), \"ShortestPathExtension\", \"CurvatureMinimizing\", \"Embedding\").";
FindLine::usage = "FindLine[graph, p1, p2] returns InfraSegment[{line}] for one maximal geodesic extension through p1 and p2. FindLine[graph, p1, p2, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Options: \"Maximality\" (\"Extension\" (default) | \"Diameter\"), Method (shared with FindSegment).";
ExtendSegment::usage = "ExtendSegment[graph, segment] extends segment to a maximal line whose middle part is segment. ExtendSegment[graph, segment, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Accepts a bare vertex sequence or an InfraSegment wrapper. Option: Method (shared with FindSegment).";
FindShell::usage = "FindShell[graph, c, r] returns InfraShell[{levelSet}] for the metric shell { v : d(c, v) == r }. FindShell[graph, c, r, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Option: Method (\"Metric\" (default) | \"Separating\" | \"Embedding\").";
FindCircle::usage = "FindCircle[graph, c, r] returns InfraCircle[{cycle}] for one separating cycle in the level-surface subgraph at radius r around c, as an open vertex sequence. FindCircle[graph, c, r, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Option: Method (\"Combinatorial\" (default) | \"Embedding\").";
FindParallel::usage = "FindParallel[graph, line, p] returns one line through p parallel to line, or $Failed. FindParallel[graph, line, p, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Parallel = constant distance to line. Option: Method (\"Metric\" (default) | \"Embedding\").";

(* ===================== EuclideanPredicates ===================== *)

SegmentQ::usage = "SegmentQ[graph, segment] tests whether segment is a geodesic path.";
LineQ::usage = "LineQ[graph, segment] tests whether segment is a maximal geodesic.";
ShellQ::usage = "ShellQ[graph, vertexSet] tests whether vertexSet is a metric shell (equidistant level surface separating its interior from exterior). Use FindShellParameters to recover {center, radius}.";
CircleQ::usage = "CircleQ[graph, cycle] tests whether the vertex sequence cycle is a metric circle (cyclic edge chain whose vertex set is a metric shell). Accepts both open and closed input.";
ParallelQ::usage = "ParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). ParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold.";
SeparatesQ::usage = "SeparatesQ[graph, vertexSet, u, v] tests whether deleting vertexSet disconnects u from v.";
FindShellParameters::usage = "FindShellParameters[graph, vertexSet] returns the list of {center, radius} pairs consistent with vertexSet being a metric shell.";
UniqueSegmentQ::usage = "UniqueSegmentQ[graph, u, v] tests whether the geodesic from u to v is unique. UniqueSegmentQ[graph] tests the geodetic property (every pair has a unique geodesic).";

(* ===================== EuclideanConstructions ===================== *)

FindMidpoint::usage = "FindMidpoint[graph, segment] returns the midpoint vertex of a segment. FindMidpoint[graph, p1, p2] returns one midpoint between p1 and p2 across all geodesics; with n / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Embedding\"; \"Spectral\", \"Resistance\" reserved).";
FindPerpendicular::usage = "FindPerpendicular[graph, line, point] returns one foot of perpendicular from point to line (Euclid I.12: isosceles base midpoint). FindPerpendicular[graph, line, point, n] / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Embedding\"; \"Spectral\", \"Resistance\" reserved).";
FindBisectingHyperplane::usage = "FindBisectingHyperplane[graph, p1, p2] returns one inclusion-minimal subset of the bisector { v : d(p1, v) == d(p2, v) } whose removal disconnects p1 from p2; with n / UpTo[n] / All controls multiplicity. A positional {lo, hi} widens the bisector to lo <= d(p1, v) - d(p2, v) <= hi.";
CompleteEquilateralTriangle::usage = "CompleteEquilateralTriangle[graph, p1, p2] returns one apex vertex equidistant from p1 and p2 at distance d(p1, p2) (Euclid I.1). With n / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default); \"Spectral\", \"Resistance\" reserved).";
InfraAngle::usage = "InfraAngle[graph, {q1, p, q2}] deletes the open ball of radius Min[d(p, q1), d(p, q2)] around p and returns the shortest q1-q2 path length in the remainder, divided by that radius. Returns Infinity when no outside path exists.";
SegmentLineAngle::usage = "SegmentLineAngle[graph, p1, p2, line] (or SegmentLineAngle[graph, segment, line]) measures the distance from a segment endpoint to a line, given the other endpoint lies on the line. The name is historical -- the value is a length, not a normalised angle. Option: Method (\"Metric\" (default); \"Spectral\" reserved).";

(* ===================== PathSpace ===================== *)

SelectPaths::usage = "SelectPaths[graph, paths, criterion] filters a path bundle by a path-space criterion (\"Central\", \"Peripheral\", \"MostVisited\", or a list of criteria folded left to right). Option: Method (\"Frechet\" (default) | \"Hausdorff\" | \"MeanFrechet\") for the metric criteria; ignored by \"MostVisited\". Accepts InfraSegment, InfraLine, InfraRay, and InfraPencil (mapped over its rays); preserves the wrapper. Operator form SelectPaths[graph, criterion, opts][paths].";
SelectCycles::usage = "SelectCycles[graph, cycles, criterion] filters a cycle bundle (criterion: \"Central\", \"Peripheral\", \"MostVisited\", \"ShortestCircumference\", \"LongestCircumference\", or a list). Option: Method (\"Frechet\" (default) | \"Hausdorff\" | \"MeanFrechet\") for the metric criteria; ignored otherwise. Accepts InfraCircle[cycles_List]. Operator form SelectCycles[graph, criterion, opts][cycles].";
EmbeddingClosestPaths::usage = "EmbeddingClosestPaths[graph, paths, {p1, p2}] keeps the paths whose drawing under GraphEmbedding is Hausdorff-closest to the straight Euclidean segment p1-p2. Operator form EmbeddingClosestPaths[graph, {p1, p2}][paths].";
EmbeddingClosestCycles::usage = "EmbeddingClosestCycles[graph, cycles, {center, radius}] keeps the cycles whose drawing under GraphEmbedding is Hausdorff-closest to the Euclidean circle of given centre and radius. Operator form EmbeddingClosestCycles[graph, {center, radius}][cycles].";
GeodesicSubgraph::usage = "GeodesicSubgraph[graph, pairs] returns the union of geodesics between the listed vertex pairs. Options: \"PathThickness\" (Hausdorff threshold for keeping multiple geodesics per pair), \"Directed\".";
PathSubgraph::usage = "PathSubgraph[graph, u, v] returns the union of all shortest u-v paths. PathSubgraph[graph, u, v, k] (or UpTo[k]) caps path length; PathSubgraph[graph, u, v, All] returns the full simple-path subgraph. Option: \"Directed\".";

(* ===================== TropicalOperations ===================== *)

TropicalPlus::usage = "TropicalPlus[x1, x2, ...] is min-plus addition: Min on scalars (identity Infinity). Listable.";
TropicalTimes::usage = "TropicalTimes[x1, x2, ...] is min-plus multiplication: Plus on scalars (identity 0). Listable.";
TropicalDot::usage = "TropicalDot[A, B] is matrix/vector product in the min-plus semiring: Inner[Plus, A, B, Min].";
TropicalMatrixPower::usage = "TropicalMatrixPower[A, k] is the k-th tropical power of A under TropicalDot. k = 0 is the tropical identity (0 on the diagonal, Infinity elsewhere).";

(* ===================== MetricAlgebra ===================== *)

MetricInterval::usage = "MetricInterval[graph, u, v] returns the vertex set { w : d(u, w) + d(w, v) == d(u, v) } -- the union of all geodesics from u to v.";
GeodesicCount::usage = "GeodesicCount[graph, u, v] returns the number of distinct geodesics from u to v, computed as (A^d)[u, v].";
DistanceMultiplicityMatrix::usage = "DistanceMultiplicityMatrix[graph] returns {D, M} where D is the distance matrix and M[i, j] is the geodesic count from vertex i to vertex j.";
DistanceMatrixQ::usage = "DistanceMatrixQ[M] tests whether M can be a graph distance matrix: square, symmetric, zero diagonal, non-negative, and tropical-idempotent (triangle inequality).";
MedianVertices::usage = "MedianVertices[graph, vs] returns the vertices minimising the sum of distances to vs. A graph is a median graph iff every triple has a unique median.";

(* ===================== Coordinatization ===================== *)

FindRadarBasis::usage = "FindRadarBasis[graph, n, m] returns up to n radar bases (resolving sets); m specifies basis sizes (All, an integer, {min, max}, or {exact}).";
RadarBasisQ::usage = "RadarBasisQ[graph, basis] tests whether basis is a radar basis (every vertex has a unique distance vector to the basis).";
RadarCoordinates::usage = "RadarCoordinates[graph, vertex, basis] returns the radar coordinates of vertex (distance vector to basis). RadarCoordinates[graph, basis] returns the Association for all vertices. Accepts InfraPoint[{...}] as the query point (singleton degenerates, multi-vertex returns the list of per-realisation vectors) and as basis entries (aggregated by option \"InfraPointAggregation\" -> Min (default), Mean, or Max).";
OrthogonalCoordinates::usage = "OrthogonalCoordinates[graph, axes, v] returns the layer-index tuple of v on each axis; OrthogonalCoordinates[graph, axes] returns the Association for all vertices; OrthogonalCoordinates[graph, c, v] auto-discovers axes through c and returns signed displacements. Axes accept InfraPoint for multi-anchor centring. Options: \"Origin\", \"SelectCoordinate\".";
FindOrthogonalAxes::usage = "FindOrthogonalAxes[graph, n] returns n mutually well-separated longest paths or $Failed; UpTo[n] returns up to n; All returns all. FindOrthogonalAxes[graph, v, n] (or InfraPoint center) requires every axis to pass through the given vertex(es). Options: \"AxisDistance\", \"MinLength\", \"MinSeparation\", \"AxisThickness\", \"RandomPick\".";

(* ===================== ProjectivePostulates ===================== *)

FindPencil::usage = "FindPencil[graph, O] returns the pencil at vertex O as InfraPencil[{InfraRay[reps_1], ...}]: K direction classes, each constituent InfraRay carrying every maximal geodesic through O sharing that direction. O accepts InfraPoint[{...}] for multi-anchor spread.";
FindRay::usage = "FindRay[graph, O, v] returns InfraRay[{ray}] for one maximal geodesic through O containing v. FindRay[graph, O, v, n] / UpTo[n] / All controls multiplicity. O and v accept InfraPoint[{...}] for multi-anchor spread.";
PencilDirections::usage = "PencilDirections[graph, O] returns the canonical lines through O, one per direction class.";
PencilCardinality::usage = "PencilCardinality[graph, O] returns the number of distinct directions in the pencil at O.";
LineCount::usage = "LineCount[graph] returns the number of distinct canonical maximal geodesics in graph.";
FindCommonLine::usage = "FindCommonLine[graph, vertices] returns InfraSegment[{line}] for one canonical line containing every listed vertex, or $Failed. With n / UpTo[n] / All controls multiplicity. Entries may be bare vertices or InfraPoint / InfraSegment / InfraRay / InfraPencil wrappers.";
FindCommonPoint::usage = "FindCommonPoint[graph, lines] returns InfraPoint[{v}] for one vertex on every listed line, or $Failed. With n / UpTo[n] / All controls multiplicity. Entries may be bare vertex sequences or InfraSegment / InfraRay / InfraPencil wrappers.";

(* ===================== ProjectivePredicates ===================== *)

SameDirectionQ::usage = "SameDirectionQ[graph, O, v, w] tests whether v and w lie in the same direction at O (some maximal geodesic through O contains both).";
CollinearQ::usage = "CollinearQ[graph, vertices] tests whether all listed vertices lie on a common line.";
ConcurrentQ::usage = "ConcurrentQ[graph, lines] tests whether all listed lines share a common vertex.";
UniquePencilQ::usage = "UniquePencilQ[graph, O] tests whether every direction at O is single-valued.";
UniqueCollinearQ::usage = "UniqueCollinearQ[graph, vertices] tests whether the listed vertices lie on a unique common line.";
UniqueConcurrentQ::usage = "UniqueConcurrentQ[graph, lines] tests whether the listed lines share exactly one common vertex.";
WhiteheadW1Q::usage = "WhiteheadW1Q[graph] tests Whitehead axiom W1: every line has at least three vertices.";
WhiteheadW2Q::usage = "WhiteheadW2Q[graph] tests Whitehead axiom W2: through any two distinct vertices there is exactly one line (geodetic property).";
WhiteheadW3Q::usage = "WhiteheadW3Q[graph] tests Whitehead axiom W3 (intersection property). O(|V|^4); use on small graphs.";
ProjectivePlaneGraphQ::usage = "ProjectivePlaneGraphQ[graph] tests whether graph is a synthetic projective plane: W1 && W2 && W3 with non-degeneracy.";

(* ===================== TarskiPostulates ===================== *)

FindTarskiSegmentExtension::usage = "FindTarskiSegmentExtension[graph, a, b, c, d] returns one vertex x with B(a, b, x) and bx == cd (Tarski A4), or $Failed. With n / UpTo[n] / All controls multiplicity. Generally fails on finite graphs.";
FindTarskiCounterexample::usage = "FindTarskiCounterexample[graph, predQ] returns one vertex tuple witnessing the failure of the Tarski axiom predicate predQ, or $Failed. With n / UpTo[n] / All caps witness count. Returns $Failed for always-True axioms and TarskiContinuityQ.";

(* ===================== TarskiPredicates ===================== *)

BetweennessQ::usage = "BetweennessQ[graph, u, w, v] tests Tarski's betweenness B(u, w, v): w lies on a geodesic from u to v.";
EquidistanceQ::usage = "EquidistanceQ[graph, a, b, c, d] tests Tarski's equidistance ab == cd: d(a, b) == d(c, d).";
TarskiStructure::usage = "TarskiStructure[graph] returns a memoized Association bundling the Tarski primitives: \"Vertices\", \"VertexIndex\", \"Distances\", \"Betweenness\", \"Equidistance\", \"Diameter\".";
TarskiBetweennessTensor::usage = "TarskiBetweennessTensor[graph] returns the sparse rank-3 tensor whose nonzero positions are the (i, j, k) with B(v_i, v_j, v_k).";
TarskiEquidistanceClasses::usage = "TarskiEquidistanceClasses[graph] returns the partition of unordered vertex pairs by distance value.";
TarskiCongruenceReflexivityQ::usage = "TarskiCongruenceReflexivityQ[graph] tests Tarski axiom A1 (ab == ba). Always True on undirected simple graphs.";
TarskiCongruenceTransitivityQ::usage = "TarskiCongruenceTransitivityQ[graph] tests Tarski axiom A2 (transitivity of ==). A tautology of equality.";
TarskiCongruenceIdentityQ::usage = "TarskiCongruenceIdentityQ[graph] tests Tarski axiom A3 (ab == cc implies a == b). Holds on connected simple graphs.";
TarskiSegmentConstructionQ::usage = "TarskiSegmentConstructionQ[graph] tests Tarski axiom A4 (segment construction). Generally False on finite graphs.";
TarskiFiveSegmentsQ::usage = "TarskiFiveSegmentsQ[graph] tests Tarski axiom A5 (five segments). Holds on median graphs. Brute O(n^8); option \"MaxTuples\" caps the search (Indeterminate if the cap is hit).";
TarskiBetweennessIdentityQ::usage = "TarskiBetweennessIdentityQ[graph] tests Tarski axiom A6 (B(a, b, a) implies a == b). Always True on connected simple graphs.";
TarskiInnerPaschQ::usage = "TarskiInnerPaschQ[graph] tests Tarski axiom A7 (Inner Pasch). Holds on median graphs; fails on cycles >= 5 and on Petersen.";
TarskiLowerDimensionQ::usage = "TarskiLowerDimensionQ[graph] tests Tarski axiom A8 (three non-collinear points exist).";
TarskiUpperDimensionQ::usage = "TarskiUpperDimensionQ[graph] tests Tarski axiom A9 (three points equidistant from two distinct points are collinear). False on graphs of effective dimension >= 3.";
TarskiEuclidAxiomQ::usage = "TarskiEuclidAxiomQ[graph] tests Tarski axiom A10 (Euclid's parallel-axiom variant). Stub: returns Indeterminate.";
TarskiContinuityQ::usage = "TarskiContinuityQ[graph] tests Tarski axiom A11 (Dedekind continuity). Always False on finite graphs.";
TarskiAxiomQ::usage = "TarskiAxiomQ[graph] returns an Association with the per-axiom result for all eleven Tarski*Q predicates.";

(* ===================== TarskiConstructions ===================== *)

FindTarskiReflection::usage = "FindTarskiReflection[graph, x, a] returns one vertex x' with B(x, a, x') and ax == ax' (reflection of x through a). With n / UpTo[n] / All controls multiplicity.";
FindTarskiMidpoint::usage = "FindTarskiMidpoint[graph, a, b] returns one vertex m with B(a, m, b) and am == mb (synthetic midpoint from B and E only). With n / UpTo[n] / All controls multiplicity. Returns { } when no synthetic midpoint exists; contrast FindMidpoint, which always returns a central interval element.";

(* ===================== TropicalPostulates ===================== *)

FindTropicalSegment::usage = "FindTropicalSegment[graph, u, v] returns one tropical segment from u to v as a sorted vertex set. With n / UpTo[n] / All controls multiplicity. In the pure-metric reading each tropical segment is the vertex set of a graph geodesic.";
FindGeodesicConvexHull::usage = "FindGeodesicConvexHull[graph, S] returns the smallest superset of S closed under FindSegment, as a sorted vertex list.";

(* ===================== TropicalPredicates ===================== *)

TropicalSegmentQ::usage = "TropicalSegmentQ[graph, S, u, v] tests whether the vertex set S realises a tropical segment from u to v.";
GeodesicallyConvexQ::usage = "GeodesicallyConvexQ[graph, S] tests whether S is closed under FindSegment.";
UniqueTropicalSegmentQ::usage = "UniqueTropicalSegmentQ[graph, u, v] tests whether the tropical segment from u to v is single-valued.";
TropicalT1Q::usage = "TropicalT1Q[graph] tests tropical axiom T1: every two vertices admit a tropical segment. Equivalent to ConnectedGraphQ.";

(* ===================== Enumeration ===================== *)

EnumerateGraphs::usage = "EnumerateGraphs[n, predQ] returns all connected n-vertex graphs from GraphData satisfying predQ. EnumerateGraphs[n, predQ, k] returns exactly k or $Failed; UpTo[k] returns up to k; All returns all. Option: \"From\" (override the default GraphData generator with a list).";

(* ===================== Example graphs ===================== *)

InfraExampleGraph::usage = "InfraExampleGraph[name, params] returns a canonical example graph from the paclet's registry, used as the running graph in guides, tutorials, and symbol-page demonstrations. InfraExampleGraph[] lists the available keys; InfraExampleGraph[name] uses default parameters. Keys cover the curvature spectrum: \"Grid\", \"RectangleMesh\", \"DiskMesh\", \"SphereMesh\", \"TriangularLattice\", \"HexagonalLattice\", \"RegularTree\", \"Cayley\", plus small named gems \"Petersen\", \"Heawood\", \"MobiusKantor\", \"Tutte\". Mesh keys forward MaxCellMeasure and AccuracyGoal to DiscretizeRegion.";

(* ===================== Scenes ===================== *)

InfraScene::usage = "InfraScene[objects, hypotheses] constructs a scene descriptor from symbolic objects and construction/assertion hypotheses. Properties: scene[\"Steps\"], [\"Constructions\"], [\"Assertions\"], [\"DependencyGraph\"].";
FindInfraScene::usage = "FindInfraScene[scene, graph] evaluates all construction steps and returns InfraInstance objects; the third argument can be n (cap step count) or an Association of pre-fixed bindings. Option: \"PruneProbability\".";
InfraInstance::usage = "InfraInstance[bindings] wraps a solved binding association from FindInfraScene. Read out via InfraInstance[bindings, sym] or InfraInstance[bindings, {sym1, sym2, ...}].";
InfraGeometricStep::usage = "InfraGeometricStep[{hyp1, hyp2, ...}] groups hypotheses into a manual construction step. InfraGeometricStep[{hyps...}, label] adds a label.";
InfraPoint::usage = "InfraPoint[] / InfraPoint[v] / InfraPoint[\"Center\"] / InfraPoint[\"Periphery\"] / InfraPoint[origin, dist] / InfraPoint[n] are scene-language constructors for any vertex / a fixed vertex / a center or periphery vertex / a vertex at a given distance / n mutually distant vertices. The wrapper form InfraPoint[{v1, ..., vk}] is the multi-realisation object returned by FindPoint. Accessors: [\"Realisations\"], [\"Length\"], [\"Expand\"], [\"First\"]; [[i]] returns a wrapped singleton; nested wrappers auto-flatten.";
InfraSegment::usage = "InfraSegment[p, q] represents geodesics between p and q. The wrapper form InfraSegment[{seg1, seg2, ...}] is the multi-realisation object returned by FindSegment / FindLine / ExtendSegment / FindParallel / FindCommonLine / FindTropicalSegment; consumed by InfraSceneHighlight (forces sequential-edge semantics). See InfraPoint for accessor conventions.";
InfraLine::usage = "InfraLine[p, q] represents maximal geodesic extensions through p and q. InfraLine[path] extends a given path. The wrapper form InfraLine[{line1, line2, ...}] is the multi-realisation object (sequential-edge semantics, same as InfraSegment).";
InfraShell::usage = "InfraShell[center, radius] represents metric shells (level surface { v : d(center, v) == radius }). The wrapper form InfraShell[{set1, set2, ...}] is returned by FindShell; consumed by InfraSceneHighlight (induced-subgraph semantics). See InfraPoint for accessor conventions.";
InfraPlane::usage = "InfraPlane[p1, p2] represents bisecting hyperplanes between p1 and p2. InfraPlane[p1, p2, {lo, hi}] widens the bisector to lo <= d(p1, v) - d(p2, v) <= hi. The wrapper form InfraPlane[{set1, set2, ...}] is returned by FindBisectingHyperplane (induced-subgraph semantics). See InfraPoint for accessor conventions.";
InfraCircle::usage = "InfraCircle[center, radius] represents metric circles (separating cycles in the level-surface subgraph). The wrapper form InfraCircle[{cyc1, cyc2, ...}] is returned by FindCircle; consumed by InfraSceneHighlight (sequential edges with auto-closure). See InfraPoint for accessor conventions.";
InfraRay::usage = "InfraRay[{ray1, ray2, ...}] is the multi-realisation wrapper for a ray at a base vertex O: maximal geodesics through O sharing direction. Returned by FindRay and as constituents of FindPencil. Consumed by InfraSceneHighlight (sequential-edge semantics). See InfraPoint for accessor conventions.";
InfraPencil::usage = "InfraPencil[{InfraRay[reps1], InfraRay[reps2], ...}] is the multi-constituent wrapper for a pencil at a base vertex O: K direction classes. Returned by FindPencil. Extra accessor [\"Rays\"] flattens every line-realisation across every direction. See InfraPoint for accessor conventions.";
InfraIntersection::usage = "InfraIntersection[obj1, obj2] represents the vertex-set intersection of two geometric objects. Used in InfraScene hypotheses.";
InfraDistance::usage = "InfraDistance[p, q] represents the graph distance d(p, q). Used in InfraScene assertions.";
InfraSegmentQ::usage = "InfraSegmentQ[s] asserts that s is a valid geodesic segment.";
InfraShellQ::usage = "InfraShellQ[vs] asserts that vs is a valid metric shell.";
InfraPlaneQ::usage = "InfraPlaneQ[h, p1, p2] asserts that h is a valid bisecting hyperplane between p1 and p2.";
InfraCircleQ::usage = "InfraCircleQ[c] asserts that c is a valid metric circle.";
InfraLineQ::usage = "InfraLineQ[s] asserts that s is a maximal geodesic.";
InfraParallelQ::usage = "InfraParallelQ[l1, l2] asserts that two lines are parallel.";
InfraIntersectQ::usage = "InfraIntersectQ[s1, s2] asserts that two sets intersect.";

(* ===================== Curvatures ===================== *)

FormanRicci::usage = "FormanRicci[graph] returns Association[edge -> kappa] with the discrete Forman-Ricci curvature. Option: Method (\"Simple\" (default), 4 - deg(u) - deg(v); \"Triangles\", adds 3 per shared neighbour).";
OllivierRicci::usage = "OllivierRicci[graph] returns Association[edge -> kappa] with the Ollivier-Ricci curvature kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v), where mu_x is uniform on N(x) and W_1 is the Wasserstein-1 distance under graph distance (alpha = 0).";
WolframRicci::usage = "WolframRicci[graph] returns Association[v -> mean_r R(v, r)] with the volume-comparison Ricci scalar averaged over the per-vertex valid radius range; the third argument can be a single radius r or a window {rmin, rmax}. Option: \"Dimension\" (Automatic (default) | integer d).";

(* ===================== Highlights / Viewers ===================== *)

InfraSceneHighlight::usage = "InfraSceneHighlight[g, multiObjects] renders a list of multi-objects diffusely on graph g, with intensity scaling by overlap within each object and color-blending across objects. Each entry is auto-classified by representation; explicit Infra* wrappers force the intended semantics, and `entry -> color` overrides the default per-head colour. Options: \"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\".";
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] is an interactive visualisation of an InfraScene on a graph; an optional third Association of pre-fixed bindings is supported. Controls: step slider, \"Fix & advance\", \"Reset\".";
PointViewer::usage = "PointViewer[g] is an interactive viewer for selecting points in graph g. PointViewer[g, sym] stores the current selection in sym.";
SegmentViewer::usage = "SegmentViewer[g] is an interactive viewer for exploring geodesic segments in graph g.";
ShellViewer::usage = "ShellViewer[g] is an interactive viewer for exploring metric shells in graph g. Method setter: \"Metric\" | \"Separating\".";
CircleViewer::usage = "CircleViewer[g] is an interactive viewer for exploring separating cycles in graph g.";
