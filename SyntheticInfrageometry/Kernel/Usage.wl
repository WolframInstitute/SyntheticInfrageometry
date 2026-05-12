Package["WolframInstitute`SyntheticInfrageometry`"]

(* Usage messages: one sentence per signature; options listed by name only.
   Full method/sub-option trees and worked examples live in the guides
   and tutorials, not here. See CLAUDE.md "Usage messages" for the rules. *)

(* ===================== InfraPoint ===================== *)

InfraPoint::usage = "InfraPoint[v] is the unary form (one vertex); InfraPoint[{v1, ..., vk}] is the multi-realisation form. Find* finders return a List of unary InfraPoint[{v}] wrappers; wrap that list with InfraPoint @ ... to collapse to the multi form via auto-flatten. Scene-language variants InfraPoint[] / InfraPoint[\"Center\"] / InfraPoint[\"Periphery\"] / InfraPoint[origin, dist] / InfraPoint[n] are constructors used inside InfraScene.";
FindPoint::usage = "FindPoint[graph] returns one vertex as {InfraPoint[{v}]}. FindPoint[graph, n] returns a List of n unary InfraPoint[{v}] wrappers or $Failed; UpTo[n] returns up to n; All returns every vertex. Options: \"From\", \"Distance\" (d for exactly distance d; {dMin, dMax} for a range, Infinity allowed in dMax), \"MaxCliques\".";
FindMidpoint::usage = "FindMidpoint[graph, segment] returns a List with one unary InfraPoint[{v}] (the midpoint of segment). FindMidpoint[graph, p1, p2] returns one midpoint between p1 and p2 across all geodesics; with n / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Tarski\" (synthetic from B and E) | \"Embedding\"; \"Spectral\", \"Resistance\" reserved).";
FindReflection::usage = "FindReflection[graph, x, a] returns a List of unary InfraPoint[{x'}] reflections of x through a, where x' satisfies B(x, a, x') and ax == ax'. With n / UpTo[n] / All controls multiplicity.";
CompleteEquilateralTriangle::usage = "CompleteEquilateralTriangle[graph, p1, p2] returns a List of unary InfraPoint[{c}] apex vertices equidistant from p1 and p2 at distance d(p1, p2) (Euclid I.1). With n / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default); \"Spectral\", \"Resistance\" reserved).";
FindCommonPoint::usage = "FindCommonPoint[graph, lines] returns a List of unary InfraPoint[{v}] wrappers for the vertices on every listed line, or $Failed. With n / UpTo[n] / All controls multiplicity. Entries may be bare vertex sequences or InfraSegment / InfraRay wrappers.";

(* ===================== InfraSegment ===================== *)

InfraSegment::usage = "InfraSegment[{path}] is the unary form (one geodesic path); InfraSegment[{path1, ..., pathk}] is the multi-realisation form. Find* finders return a List of unary InfraSegment[{path}] wrappers; wrap that list with InfraSegment @ ... to collapse to multi via auto-flatten. Scene-language constructor InfraSegment[p, q] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (sequential-edge semantics).";
FindSegment::usage = "FindSegment[graph, p1, p2] returns {InfraSegment[{path}]} for the shortest path. FindSegment[graph, p1, p2, n] returns a List of n unary InfraSegment[{path}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Endpoints accept InfraPoint[{...}] for multi-anchor spread. Options: Method (\"Shortest\" (default), \"ShortestPathExtension\", \"CurvatureMinimizing\", \"Embedding\").";
ExtendSegment::usage = "ExtendSegment[graph, segment] extends segment to a maximal line whose middle part is segment, returned as a List of unary InfraSegment[{line}] wrappers. ExtendSegment[graph, segment, n] / UpTo[n] / All controls multiplicity. Accepts a bare vertex sequence or an InfraSegment wrapper. Option: Method (shared with FindSegment). ExtendSegment[graph, a, b, c, d, n] is the Tarski synthetic-extension form: returns a List of unary InfraPoint[{x}] wrappers for vertices x with B(a, b, x) and bx == cd.";
SegmentQ::usage = "SegmentQ[graph, segment] tests whether segment is a geodesic path.";
UniqueSegmentQ::usage = "UniqueSegmentQ[graph, u, v] tests whether the geodesic from u to v is unique. UniqueSegmentQ[graph] tests the geodetic property (every pair has a unique geodesic).";

(* ===================== InfraLine ===================== *)

FindLine::usage = "FindLine[graph, p1, p2] returns {InfraSegment[{line}]} for one maximal geodesic extension through p1 and p2. FindLine[graph, p1, p2, n] returns a List of n unary InfraSegment[{line}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Options: \"Maximality\" (\"Extension\" (default) | \"Diameter\"), Method (shared with FindSegment).";
FindParallel::usage = "FindParallel[graph, line, p] returns a List of unary InfraSegment[{line}] wrappers for parallels through p (parallel = constant distance to line), or $Failed. FindParallel[graph, line, p, n] / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Embedding\").";
FindPerpendicular::usage = "FindPerpendicular[graph, line, point] returns a List of unary InfraPoint[{foot}] wrappers for the feet of perpendiculars from point to line (Euclid I.12: isosceles base midpoint). FindPerpendicular[graph, line, point, n] / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Embedding\"; \"Spectral\", \"Resistance\" reserved).";
FindCommonLine::usage = "FindCommonLine[graph, vertices] returns a List of unary InfraSegment[{line}] wrappers for the canonical lines containing every listed vertex, or $Failed. With n / UpTo[n] / All controls multiplicity. Entries may be bare vertices or InfraPoint / InfraSegment / InfraRay wrappers.";
SegmentLineAngle::usage = "SegmentLineAngle[graph, p1, p2, line] (or SegmentLineAngle[graph, segment, line]) measures the distance from a segment endpoint to a line, given the other endpoint lies on the line. The name is historical -- the value is a length, not a normalised angle. Option: Method (\"Metric\" (default); \"Spectral\" reserved).";
LineQ::usage = "LineQ[graph, segment] tests whether segment is a maximal geodesic.";
ParallelQ::usage = "ParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). ParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold.";
PencilDirections::usage = "PencilDirections[graph, O] returns the canonical maximal geodesics through O, one per projective direction class at O.";
PencilCardinality::usage = "PencilCardinality[graph, O] returns the number of distinct direction classes at O.";
LineCount::usage = "LineCount[graph] returns the number of distinct canonical maximal geodesics in graph.";

(* ===================== InfraShell ===================== *)

InfraShell::usage = "InfraShell[{set}] is the unary form (one metric shell vertex set); InfraShell[{set1, ..., setk}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraShell @ ... to collapse to multi via auto-flatten. Scene-language constructor InfraShell[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (induced-subgraph semantics).";
FindShell::usage = "FindShell[graph, c, r] returns {InfraShell[{levelSet}]} for the metric shell { v : d(c, v) == r }. FindShell[graph, c, r, n] returns a List of n unary InfraShell[{set}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Option: Method (\"Metric\" (default) | \"Separating\" | \"Embedding\").";
FindShellParameters::usage = "FindShellParameters[graph, vertexSet] returns the list of {center, radius} pairs consistent with vertexSet being a metric shell.";
ShellQ::usage = "ShellQ[graph, vertexSet] tests whether vertexSet is a metric shell (equidistant level surface separating its interior from exterior). Use FindShellParameters to recover {center, radius}.";
SeparatesQ::usage = "SeparatesQ[graph, vertexSet, u, v] tests whether deleting vertexSet disconnects u from v.";

(* ===================== InfraCircle ===================== *)

InfraCircle::usage = "InfraCircle[{cycle}] is the unary form; InfraCircle[{cycle1, ..., cyclek}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraCircle @ ... to collapse to multi via auto-flatten. Scene-language constructor InfraCircle[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (sequential edges with auto-closure).";
FindCircle::usage = "FindCircle[graph, c, r] returns {InfraCircle[{cycle}]} for one separating cycle in the level-surface subgraph at radius r around c, as an open vertex sequence. FindCircle[graph, c, r, n] returns a List of n unary InfraCircle[{cycle}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Option: Method (\"Combinatorial\" (default) | \"Embedding\").";
CircleQ::usage = "CircleQ[graph, cycle] tests whether the vertex sequence cycle is a metric circle (cyclic edge chain whose vertex set is a metric shell). Accepts both open and closed input.";

(* ===================== InfraPlane ===================== *)

InfraPlane::usage = "InfraPlane[{set}] is the unary form (one bisecting hyperplane); InfraPlane[{set1, ..., setk}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraPlane @ ... to collapse to multi via auto-flatten. Scene-language constructors InfraPlane[p1, p2] and InfraPlane[p1, p2, {lo, hi}] are used inside InfraScene.";
FindBisectingHyperplane::usage = "FindBisectingHyperplane[graph, p1, p2] returns a List of unary InfraPlane[{set}] wrappers for the inclusion-minimal subsets of the bisector { v : d(p1, v) == d(p2, v) } whose removal disconnects p1 from p2; with n / UpTo[n] / All controls multiplicity. A positional {lo, hi} widens the bisector to lo <= d(p1, v) - d(p2, v) <= hi.";

(* ===================== InfraRay ===================== *)

InfraRay::usage = "InfraRay[{ray}] is the unary form (one pointed half-line from a base vertex O); InfraRay[{ray1, ..., rayk}] is the multi-realisation form. Each ray is a vertex sequence {O, ..., w} with w an inextensible endpoint. Find* returns a List of unary wrappers; wrap with InfraRay @ ... to collapse to multi via auto-flatten. Consumed by InfraSceneHighlight (sequential-edge semantics).";
FindRay::usage = "FindRay[graph, O, v] returns {InfraRay[{ray}]} for one pointed half-line from O in v's direction: the half of a maximal geodesic line through O and v starting at O and ending at the far inextensible endpoint. FindRay[graph, O, v, n] returns a List of n unary InfraRay[{ray}] wrappers or $Failed; UpTo[n] / All controls multiplicity. O and v accept InfraPoint[{...}] for multi-anchor spread.";

(* ===================== InfraRevolution ===================== *)

InfraRevolution::usage = "InfraRevolution[{set}] wraps a rotational vertex set around an axis. Scene-language constructor InfraRevolution[axis, profile] is used inside InfraScene; consumed by InfraSceneHighlight (induced-subgraph semantics).";
FindRevolution::usage = "FindRevolution[graph, axis, profile] returns InfraRevolution[{set}] for the rotational vertex set around axis with given radius profile: the union over axis vertices a of the Voronoi slab of a intersected with the per-vertex ball (Solid) or sphere (Surface) of radius profile[a]. The profile may be NumericQ (constant), a List of length |axis|, an Association vertex -> radius, or any callable applied to index 1..|axis|. Option: \"Form\" (\"Surface\" (default) | \"Solid\").";
FindCylinder::usage = "FindCylinder[graph, axis, r] returns InfraRevolution[{set}] for the constant-radius rotational set around axis. Inherits FindRevolution options.";
FindCone::usage = "FindCone[graph, axis, slope] returns InfraRevolution[{set}] for the cone of given slope with apex at axis[[1]]: radii are slope * Range[0, Length[axis] - 1]. Option \"Apex\" (First (default) | Last) flips the apex end; inherits remaining options from FindRevolution.";
RevolutionQ::usage = "RevolutionQ[graph, vs, axis, profile] tests whether vs equals the rotational vertex set around axis with the given radius profile. Option \"Form\" matches FindRevolution.";

(* ===================== EuclideanSpace ===================== *)

InfraScalarProduct::usage = "InfraScalarProduct[graph, o, u, v] returns the base-point-relative scalar product of u and v with respect to o. Option: Method (\"Schoenberg\" (default) -- direct distance formula (d(o,u)^2 + d(o,v)^2 - d(u,v)^2)/2; \"Parallelogram\" -- polarization identity (||u+v||^2 - ||u-v||^2)/4 via FindInfraLinearCombination).";
FindInfraLinearCombination::usage = "FindInfraLinearCombination[graph, o, {{lambda1, u1}, {lambda2, u2}, ...}] returns a List of unary InfraPoint[{v}] wrappers for the multi-valued vertex realisation of sum_i lambda_i u_i with base point o, computed as scaled-then-pairwise-summed left-to-right. With n / UpTo[n] / All controls multiplicity. Options: \"ScaleMethod\" (Automatic (default), \"Metric\", \"Line\", \"Midpoint\"); \"SumMethod\" (\"Metric\" (default), \"Parallel\").";
InfraAngle::usage = "InfraAngle[graph, {q1, p, q2}] returns an angle at p. Option: Method (\"PunchOut\" (default) -- delete the open ball of radius Min[d(p, q1), d(p, q2)] around p and return the shortest q1-q2 path length outside, divided by the radius; \"Comparison\" -- the Alexandrov comparison-triangle angle at p in the Euclidean k = 0 model via the law of cosines; {\"Comparison\", \"Curvature\" -> k} -- the same angle in M_k^2 (spherical / Euclidean / hyperbolic) for arbitrary k).";

(* ===================== InfraCurveGeometry ===================== *)

TurningAngles::usage = "TurningAngles[graph, path] returns the list of discrete exterior angles Pi - InfraAngle[graph, {v_{i-1}, v_i, v_{i+1}}] at each interior vertex of the polygonal curve path = {v_1, ..., v_k}; closed cycles (First[path] == Last[path]) include the wrap-around triple.";
TotalCurvature::usage = "TotalCurvature[graph, path] returns the discrete total curvature Total @ TurningAngles[graph, path] of the polygonal curve.";
TotalAbsoluteCurvature::usage = "TotalAbsoluteCurvature[graph, path] returns Total @ Abs @ TurningAngles[graph, path], the discrete analogue of the Fenchel-side integral of |kappa|.";
TurningNumber::usage = "TurningNumber[graph, cycle] returns TotalCurvature[graph, cycle] / (2 Pi); integer in the smooth planar case, a real-valued empirical quantity on graphs.";

(* ===================== AlexandrovGeometry ===================== *)

ComparisonTriangle::usage = "ComparisonTriangle[a, b, c] returns the Wolfram Triangle in R^2 with side lengths {a, b, c} (a opposite p, b opposite q, c opposite r). ComparisonTriangle[a, b, c, \"Curvature\" -> k] for k != 0 returns InfraComparisonTriangle[<|\"Sides\" -> ..., \"Curvature\" -> k, \"Angles\" -> {alpha_p, alpha_q, alpha_r}|>] in M_k^2. ComparisonTriangle[graph, p, q, r, \"Curvature\" -> k] reads the side lengths from the graph.";
InfraComparisonTriangle::usage = "InfraComparisonTriangle[<|...|>] is the wrapper head for non-Euclidean comparison triangles returned by ComparisonTriangle. Accessors: [\"Sides\"], [\"Curvature\"], [\"Angles\"].";
CATInequalityQ::usage = "CATInequalityQ[graph, {p, q, r}, k : 0] tests whether the geodesic triangle on {p, q, r} satisfies the CAT(k) thinness inequality. Option Method (\"ApexSide\" (default) -- d(apex, x) <= d_k_bar(apex', x') for every interior vertex x on the side opposite to each apex; \"TwoRays\" -- d(x, y) <= d_k_bar(x', y') for every cross-ray pair (x, y) on the two rays emanating from each apex). Returns Indeterminate when k > 0 and the triangle perimeter exceeds 2 Pi / Sqrt[k].";
InfraCurvature::usage = "InfraCurvature[graph, v] returns the local Alexandrov upper-curvature bound at v: L^2 times the supremum of per-triangle CAT bounds over all triangles whose three vertices sit inside B_L(v), with L = GraphDiameter[graph] by default. Option: \"Radius\" (Automatic | Integer L) selects the ball radius; reported curvature is rescaled by L^2 to match the (edge / L)^-2 unit convention. InfraCurvature[graph] returns an Association[v -> kappa_v] over all vertices.";

(* ===================== PathSpace ===================== *)

SelectPath::usage = "SelectPath[graph, paths] returns one path drawn from the bundle treated as a finite metric space in path-space. SelectPath[graph, paths, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole pool. Options mirror FindPoint: \"From\" (All (default), \"Center\", \"Periphery\", \"MostVisited\", anchor -> spec, InfraSegment[{...}] -> spec), \"Distance\" (None (default), \"Max\", d, {dMin, dMax}), \"Metric\" (\"Hausdorff\" (default), \"Frechet\", \"MeanFrechet\"), \"MaxCliques\". Accepts InfraSegment and InfraRay wrappers; preserves the wrapper. Operator form SelectPath[graph, n, opts][paths].";
SelectCycle::usage = "SelectCycle[graph, cycles] returns one cycle drawn from the bundle in path-space. SelectCycle[graph, cycles, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole pool. Options mirror SelectPath, with two extra \"From\" values: \"ShortestCircumference\", \"LongestCircumference\". Path-space metric distances factor cyclic rotation so they are rotation-invariant. Accepts InfraCircle[cycles_List]; preserves the wrapper. Operator form SelectCycle[graph, n, opts][cycles].";
EmbeddingClosestPaths::usage = "EmbeddingClosestPaths[graph, paths, {p1, p2}] keeps the paths whose drawing under GraphEmbedding is Hausdorff-closest to the straight Euclidean segment p1-p2. Operator form EmbeddingClosestPaths[graph, {p1, p2}][paths].";
EmbeddingClosestCycles::usage = "EmbeddingClosestCycles[graph, cycles, {center, radius}] keeps the cycles whose drawing under GraphEmbedding is Hausdorff-closest to the Euclidean circle of given centre and radius. Operator form EmbeddingClosestCycles[graph, {center, radius}][cycles].";
GeodesicGraph::usage = "GeodesicGraph[graph, c] returns the BFS DAG rooted at c: a directed graph with edge u -> v whenever d(c, v) = d(c, u) + 1 and u-v is an edge of graph. Sinks are peripheral vertices reachable from c; directed paths c -> sink are exactly the maximal geodesics from c. Accepts InfraPoint[{c1, ..., ck}] for multi-source BFS. Option: \"AxisLength\" (truncate at depth k; default All).";
GeodesicSubgraph::usage = "GeodesicSubgraph[graph, pairs] returns the union of geodesics between the listed vertex pairs. Options: \"PathThickness\" (Hausdorff threshold for keeping multiple geodesics per pair), \"Directed\".";
PathSubgraph::usage = "PathSubgraph[graph, u, v] returns the union of all shortest u-v paths. PathSubgraph[graph, u, v, k] (or UpTo[k]) caps path length; PathSubgraph[graph, u, v, All] returns the full simple-path subgraph. Option: \"Directed\".";

(* ===================== Homotopy ===================== *)

InfraHomotopy::usage = "InfraHomotopy[{chain}] is the unary form: one homotopy chain {p0, p1, ..., pk} of intermediate paths produced by k elementary face moves from p0 to pk. InfraHomotopy[{chain1, ..., chaink}] is the multi-realisation form. Find* finders return a List of unary InfraHomotopy[{chain}] wrappers; wrap that list with InfraHomotopy @ ... to collapse to multi via auto-flatten.";
FindHomotopy::usage = "FindHomotopy[graph, p1, p2] returns {InfraHomotopy[{chain}]} exhibiting a face-move chain from path p1 to path p2 in graph, or {} if none is found within the search bounds. FindHomotopy[graph, p1, p2, n] returns a List of n unary InfraHomotopy[{chain}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Endpoints accept InfraSegment[{...}] for multi-anchor spread. Options: \"Faces\" (Automatic, an integer k, a list of integers, All, \"Fundamental\", or a list of vertex cycles), \"MaxLength\" (walk-length cap during BFS), \"MaxMoves\" (chain-depth cap), Method (\"BFS\" only).";
FindNullHomotopy::usage = "FindNullHomotopy[graph, cycle] returns {InfraHomotopy[{chain}]} exhibiting a face-move chain contracting cycle to its base vertex, or {} if cycle is not null-homotopic within the search bounds. With n / UpTo[n] / All controls multiplicity. Accepts InfraCircle[{...}] for multi-cycle spread. Inherits FindHomotopy options.";
HomotopicQ::usage = "HomotopicQ[graph, p1, p2] tests whether paths p1 and p2 are homotopic under the chosen face structure. Spreads over multi-realisation inputs as a Cartesian-AllTrue conjunction. Inherits FindHomotopy options.";
NullHomotopicQ::usage = "NullHomotopicQ[graph, cycle] tests whether cycle bounds a 2-disk built from the chosen faces (homotopic to a constant walk). Inherits FindHomotopy options.";
ReducePath::usage = "ReducePath[graph, path] applies length-non-increasing face moves to path until no shortening move remains, returning a short representative of its homotopy class. Option: \"Faces\" (default {2}, backtrack reduction only).";

(* ===================== MetricGeometry ===================== *)

MetricInterval::usage = "MetricInterval[graph, u, v] returns the vertex set { w : d(u, w) + d(w, v) == d(u, v) } -- the union of all geodesics from u to v.";
GeodesicMultiplicity::usage = "GeodesicMultiplicity[graph, u, v] returns the number of distinct geodesics from u to v, computed as (A^d)[u, v] where d = GraphDistance[graph, u, v].";
GeodesicMultiplicityMatrix::usage = "GeodesicMultiplicityMatrix[graph] returns {D, M} where D is the distance matrix and M[i, j] is the number of geodesics from vertex i to vertex j.";
MedianVertices::usage = "MedianVertices[graph, vs] returns the vertices minimising the sum of distances to vs. A graph is a median graph iff every triple has a unique median.";
FindGeodesicConvexHull::usage = "FindGeodesicConvexHull[graph, S] returns the smallest superset of S closed under MetricInterval, as a sorted vertex list. Graph-intrinsic shadow of tropical convexity (see Wiki/Concepts/TropicalConvexity).";
GeodesicallyConvexQ::usage = "GeodesicallyConvexQ[graph, S] tests geodesic convexity of S. Option Method (\"Strong\" (default): every geodesic between any pair of S lies in S; \"Weak\": some geodesic between each pair lies in S).";

(* ===================== Coordinatization ===================== *)

ResistanceCoordinates::usage = "ResistanceCoordinates[graph] returns the Association v -> Phi(v) of resistance-matching spectral coordinates Phi(v) = (phi_i(v) / Sqrt[lambda_i])_{i: lambda_i > 0}, satisfying ||Phi(u) - Phi(v)||^2 == EffectiveResistance(u, v). ResistanceCoordinates[graph, v] returns one coordinate vector; an InfraPoint query returns the per-realisation list. Options: \"Rescaling\" (\"ResistanceMatching\" (default), \"None\", \"Diffusion\" -> t), \"Dimension\" (Automatic, Integer, UpTo[k], All), \"Origin\" (None or vertex / InfraPoint).";

FindRadarBasis::usage = "FindRadarBasis[graph, n, m] returns up to n radar bases (resolving sets); m specifies basis sizes (All, an integer, {min, max}, or {exact}).";
RadarBasisQ::usage = "RadarBasisQ[graph, basis] tests whether basis is a radar basis (every vertex has a unique distance vector to the basis).";
RadarCoordinates::usage = "RadarCoordinates[graph, basis, vertex] returns the radar coordinates of vertex (distance vector to basis). RadarCoordinates[graph, basis] returns the Association for all vertices, so RadarCoordinates[graph, basis][vertex] is the natural operator form. Accepts InfraPoint[{...}] as the query point (singleton degenerates, multi-vertex returns the list of per-realisation vectors) and as basis entries (aggregated by option \"InfraPointAggregation\" -> Min (default), Mean, or Max).";
OrthogonalCoordinates::usage = "OrthogonalCoordinates[graph, c, {a1, a2, ...}, v] returns the Z-valued displacement of v on each axis ai through the centre c; OrthogonalCoordinates[graph, c, {a1, a2, ...}] returns the Association for all vertices. Centre c is a vertex or InfraPoint; each axis ai is an InfraSegment, InfraLine, or vertex sequence. Option \"SelectCoordinate\" (projection-tie reducer applied to the shifted tied list ix_v - k: \"Centered\" (default; 0 if 0 in shifted, else Round[Median]), Min, Max, Mean, Median, All, or any user function).";
FindOrthogonalFrame::usage = "FindOrthogonalFrame[graph, c, axisLength] returns one orthogonal frame at centre c with each axis half-depth constrained by axisLength (All | n_Integer | UpTo[n] | {min, max}): a list of InfraSegment axes (full metric lines through c, c strictly interior), mutually perpendicular at c. FindOrthogonalFrame[graph, c, axisLength, n] returns exactly n distinct frames or $Failed; UpTo[n] returns up to n; All returns every frame. Centre is a vertex or InfraPoint[{c1, ..., ck}]. Options: Method (Automatic = \"Exhaustive\", or \"Greedy\"), \"AxisCount\" (Automatic | k | UpTo[k] | All), \"BranchSampleSize\" (Exhaustive subsample cap), \"SelectCoordinate\" (projection-tie reducer; perpendicularity at c is defined as the per-axis coord being 0 under this reducer applied to the shifted tied list ix_w - k; \"Centered\" (default; 0 if 0 in shifted, else Round[Median]), Min, Max, Mean, Median, All, ...). For the no-centre form use FindSpanningAxes.";
FindSpanningAxes::usage = "FindSpanningAxes[graph, n] returns n mutually well-separated longest geodesics across graph (greedy, no fixed center) or $Failed; UpTo[n] returns up to n; All returns every axis above the separation threshold. Options: \"AxisDistance\" (\"MinEndpoint\" | \"Hausdorff\" | \"Separation\"), \"MinLength\", \"MinSeparation\", \"AxisThickness\", \"RandomPick\". For axes through a fixed center vertex use FindOrthogonalFrame.";

(* ===================== TarskiGeometry ===================== *)

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
FindTarskiCounterexample::usage = "FindTarskiCounterexample[graph, predQ] returns one vertex tuple witnessing the failure of the Tarski axiom predicate predQ, or $Failed. With n / UpTo[n] / All caps witness count. Returns $Failed for always-True axioms and TarskiContinuityQ.";

(* ===================== ProjectiveGeometry ===================== *)

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

(* ===================== Enumeration ===================== *)

EnumerateGraphs::usage = "EnumerateGraphs[n, predQ] returns all connected n-vertex graphs from GraphData satisfying predQ. EnumerateGraphs[n, predQ, k] returns exactly k or $Failed; UpTo[k] returns up to k; All returns all. Option: \"From\" (override the default GraphData generator with a list).";

(* ===================== Example graphs ===================== *)

PunchHole::usage = "PunchHole[g, r] removes the closed r-ball around a random vertex; PunchHole[g, c -> r] removes the closed r-ball around vertex c. For multiple holes, Fold[PunchHole, g, list].";

TorusTessellation::usage = "TorusTessellation[shape, {m, n}] returns the vertex-transitive flat-torus Cayley graph carrying the regular {p, q}-tessellation, where shape is \"Rectangular\" ({4, 4}, 4-regular), \"Triangular\" ({3, 6}, 6-regular), or \"Hexagonal\" ({6, 3}, 3-regular).";

(* ===================== Scenes ===================== *)

InfraScene::usage = "InfraScene[objects, hypotheses] constructs a scene descriptor from symbolic objects and construction/assertion hypotheses. Properties: scene[\"Steps\"], [\"Constructions\"], [\"Assertions\"], [\"DependencyGraph\"].";
FindInfraScene::usage = "FindInfraScene[scene, graph] evaluates all construction steps and returns InfraInstance objects; the third argument can be n (cap step count) or an Association of pre-fixed bindings. Option: \"PruneProbability\".";
InfraInstance::usage = "InfraInstance[bindings] wraps a solved binding association from FindInfraScene. Read out via InfraInstance[bindings, sym] or InfraInstance[bindings, {sym1, sym2, ...}].";
InfraGeometricStep::usage = "InfraGeometricStep[{hyp1, hyp2, ...}] groups hypotheses into a manual construction step. InfraGeometricStep[{hyps...}, label] adds a label.";
InfraLine::usage = "InfraLine[p, q] represents maximal geodesic extensions through p and q. InfraLine[path] extends a given path. Multi-realisations are returned as InfraSegment[{...}] (the only path-shaped wrapper).";
InfraIntersection::usage = "InfraIntersection[obj1, obj2] represents the vertex-set intersection of two geometric objects. Used in InfraScene hypotheses.";
InfraDistance::usage = "InfraDistance[g, p, q] is the graph distance between p and q in g, where each is a bare vertex or any Infra* wrapper (InfraPoint, InfraSegment, InfraLine, InfraRay, InfraCircle, InfraShell, InfraPlane); the result is aggregated over the cross-product of underlying vertex sets via the \"Aggregation\" option (Min default, also Max / Mean / any List -> Number). InfraDistance[p, q] without a graph is also recognised inside InfraScene assertions.";
InfraSegmentQ::usage = "InfraSegmentQ[s] asserts that s is a valid geodesic segment.";
InfraShellQ::usage = "InfraShellQ[vs] asserts that vs is a valid metric shell.";
InfraPlaneQ::usage = "InfraPlaneQ[h, p1, p2] asserts that h is a valid bisecting hyperplane between p1 and p2.";
InfraCircleQ::usage = "InfraCircleQ[c] asserts that c is a valid metric circle.";
InfraLineQ::usage = "InfraLineQ[s] asserts that s is a maximal geodesic.";
InfraParallelQ::usage = "InfraParallelQ[l1, l2] asserts that two lines are parallel.";
InfraIntersectQ::usage = "InfraIntersectQ[s1, s2] asserts that two sets intersect.";
InfraRevolutionQ::usage = "InfraRevolutionQ[vs, axis, profile] asserts that vs is the rotational vertex set around axis with the given radius profile.";

(* ===================== Highlights / Viewers ===================== *)

$InfraPointColor::usage   = "Default highlight color for InfraPoint objects.";
$InfraSegmentColor::usage = "Default highlight color for InfraSegment objects.";
$InfraShellColor::usage   = "Default highlight color for InfraShell objects.";
$InfraPlaneColor::usage   = "Default highlight color for InfraPlane objects.";
$InfraCircleColor::usage  = "Default highlight color for InfraCircle objects.";
$InfraRayColor::usage     = "Default highlight color for InfraRay objects.";

InfraSceneHighlight::usage = "InfraSceneHighlight[g, multiObjects] renders a list of multi-objects diffusely on graph g, with intensity scaling by overlap within each object and color-blending across objects. Each entry is auto-classified by representation; explicit Infra* wrappers force the intended semantics, and `entry -> color` overrides the default per-head colour. Options: \"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\".";
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] is an interactive visualisation of an InfraScene on a graph; an optional third Association of pre-fixed bindings is supported. Controls: step slider, \"Fix & advance\", \"Reset\".";
PointViewer::usage = "PointViewer[g] is an interactive viewer for selecting points in graph g. PointViewer[g, sym] stores the current selection in sym.";
SegmentViewer::usage = "SegmentViewer[g] is an interactive viewer for exploring geodesic segments in graph g.";
ShellViewer::usage = "ShellViewer[g] is an interactive viewer for exploring metric shells in graph g. Method setter: \"Metric\" | \"Separating\".";
CircleViewer::usage = "CircleViewer[g] is an interactive viewer for exploring separating cycles in graph g.";
