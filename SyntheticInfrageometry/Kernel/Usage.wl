Package["WolframInstitute`SyntheticInfrageometry`"]

(* Usage messages: one sentence per signature; options listed by name only.
   Full method/sub-option trees and worked examples live in the guides
   and tutorials, not here. See CLAUDE.md "Usage messages" for the rules. *)

(* ===================== InfraPoint ===================== *)

InfraPoint::usage = "InfraPoint[v] is the unary form (one vertex); InfraPoint[{v1, ..., vk}] is the multi-realisation form. Find* finders return a List of unary InfraPoint[{v}] wrappers; wrap that list with InfraPoint @ ... to collapse to the multi form via auto-flatten. Scene-language variants InfraPoint[] / InfraPoint[\"Center\"] / InfraPoint[\"Periphery\"] / InfraPoint[origin, dist] / InfraPoint[n] are constructors used inside InfraScene.";
FindInfraPoint::usage = "FindInfraPoint[graph] returns one vertex as {InfraPoint[{v}]}. FindInfraPoint[graph, n] returns a List of n unary InfraPoint[{v}] wrappers or $Failed; UpTo[n] returns up to n; All returns every vertex. Options: \"From\", \"Distance\" (d for exactly distance d; {dMin, dMax} for a range, Infinity allowed in dMax), \"MaxCliques\".";
FindInfraMidpoint::usage = "FindInfraMidpoint[graph, segment] returns a List with one unary InfraPoint[{v}] (the midpoint of segment). FindInfraMidpoint[graph, p1, p2] returns one midpoint between p1 and p2 across all geodesics; with n / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Tarski\" (synthetic from B and E) | \"Embedding\").";
FindInfraReflection::usage = "FindInfraReflection[graph, x, a] returns a List of unary InfraPoint[{x'}] reflections of x through a, where x' satisfies B(x, a, x') and ax == ax'. With n / UpTo[n] / All controls multiplicity.";
CompleteInfraEquilateralTriangle::usage = "CompleteInfraEquilateralTriangle[graph, p1, p2] returns a List of unary InfraPoint[{c}] apex vertices equidistant from p1 and p2 at distance d(p1, p2) (Euclid I.1). With n / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default)).";
FindInfraCommonPoint::usage = "FindInfraCommonPoint[graph, lines] returns a List of unary InfraPoint[{v}] wrappers for the vertices on every listed line, or $Failed. With n / UpTo[n] / All controls multiplicity. Entries may be bare vertex sequences or InfraSegment / InfraRay wrappers.";
SelectInfraPoint::usage = "SelectInfraPoint[graph, vertices] returns one vertex drawn from the bundle treated as a finite metric space under graph distance. SelectInfraPoint[graph, vertices, n] returns exactly n unary InfraPoint[{v}] wrappers or $Failed; UpTo[n] returns up to n; All returns the whole filtered pool. Options mirror FindInfraPoint: \"From\" (All (default), \"Center\", \"Periphery\", anchor -> spec, InfraPoint[{...}] -> spec, vertex, list), \"Distance\" (None (default), \"Max\", d, {dMin, dMax}), \"MaxCliques\". Accepts InfraPoint[{...}]; preserves the wrapper. Operator form SelectInfraPoint[graph, n, opts][vertices].";
InfraReachableQ::usage = "InfraReachableQ[graph, p1, p2] tests whether some realisation of p1 lies in the same connected component as some realisation of p2. Accepts InfraPoint[{...}], a bare vertex, or a list of vertices.";

(* ===================== InfraSegment ===================== *)

InfraSegment::usage = "InfraSegment[{path}] is the unary form (one geodesic path); InfraSegment[{path1, ..., pathk}] is the multi-realisation form. Find* finders return a List of unary InfraSegment[{path}] wrappers; wrap that list with InfraSegment @ ... to collapse to multi via auto-flatten. Scene-language constructor InfraSegment[p, q] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (sequential-edge semantics).";
FindInfraSegment::usage = "FindInfraSegment[graph, p1, p2] returns {InfraSegment[{path}]} for the shortest path. FindInfraSegment[graph, p1, p2, n] returns a List of n unary InfraSegment[{path}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Endpoints accept InfraPoint[{...}] for multi-anchor spread. Options: Method (\"ShortestPath\" (default), \"ShortestPathExtension\", \"CurvatureMinimizing\", \"Embedding\").";
ExtendInfraSegment::usage = "ExtendInfraSegment[graph, segment] extends segment to a maximal line whose middle part is segment, returned as a List of unary InfraLine[{line}] wrappers. ExtendInfraSegment[graph, segment, n] / UpTo[n] / All controls multiplicity. Accepts a bare vertex sequence or an InfraSegment wrapper. Options: Method (\"ShortestPath\" (default), \"ShortestPathExtension\", \"CurvatureMinimizing\", \"Embedding\", \"Greedy\"), \"Pruning\" (caps prefix/suffix enumeration on \"ShortestPath\"). ExtendInfraSegment[graph, a, b, c, d, n] is the Tarski synthetic-extension form: returns a List of unary InfraPoint[{x}] wrappers for vertices x with B(a, b, x) and bx == cd.";
InfraPathQ::usage = "InfraPathQ[graph, walk] tests whether walk is a simple path in graph (consecutive adjacency and no repeated vertices). Hierarchy: InfraPathQ \[Superset] InfraSegmentQ \[Superset] InfraLineQ.";
InfraPath::usage = "InfraPath[{walk}] is the unary form (one simple path); InfraPath[{walk1, ..., walkk}] is the multi-realisation form. FindInfraPath finders return a List of unary InfraPath[{walk}] wrappers; wrap that list with InfraPath @ ... to collapse to multi via auto-flatten. Rendered like InfraSegment (sequential-edge semantics).";
FindInfraPath::usage = "FindInfraPath[graph, p1, p2] returns {InfraPath[{walk}]} for one simple path between p1 and p2. FindInfraPath[graph, p1, p2, kspec] restricts the length: kspec is k (length <= k), {k} (length == k), {kmin, kmax} (range), or Infinity (any length, default). FindInfraPath[graph, p1, p2, kspec, n] / UpTo[n] / All controls multiplicity. Endpoints accept InfraPoint[{...}] for multi-anchor spread. Delegates enumeration to the Wolfram built-in FindPath.";
ExtendInfraPath::usage = "ExtendInfraPath[graph, path] extends a walk by a per-step geometric rule and returns a List of unary InfraPath[{walk}] wrappers. ExtendInfraPath[graph, path, n] / UpTo[n] / All controls multiplicity; the multi-realisation InfraPath wrapper spreads. Options: Method (\"CurvatureMinimizing\" (default; sub-option \"Curvature\"), \"ShortestPath\" (sub-option \"Window\"), \"LongestPath\" (sub-options \"Window\", \"Aggregation\" -> \"Lex\" | \"Sum\")), \"Length\" (Automatic (default; extend until inextensible) or integer steps per requested side), \"Side\" (\"Both\" (default), \"Forward\", \"Backward\"), \"Pruning\".";
ConcatenateInfraPath::usage = "ConcatenateInfraPath[path1, path2] returns a List of unary InfraPath[{walk}] wrappers for every compatible pair (walk1 from path1, walk2 from path2) with Last[walk1] === First[walk2], concatenated as Join[walk1, Rest[walk2]]. ConcatenateInfraPath[path1, path2, n] / UpTo[n] / All controls multiplicity (default All). Both arguments accept multi-realisation InfraPath[{...}] wrappers or lists of unary wrappers.";
InfraSegmentQ::usage = "InfraSegmentQ[graph, segment] tests whether segment is a geodesic path.";
UniqueInfraSegmentQ::usage = "UniqueInfraSegmentQ[graph, u, v] tests whether the geodesic from u to v is unique. UniqueInfraSegmentQ[graph] tests the geodetic property (every pair has a unique geodesic).";

(* ===================== InfraLine ===================== *)

InfraLine::usage = "InfraLine[{line}] is the unary form (one maximal geodesic); InfraLine[{line1, ..., linek}] is the multi-realisation form. FindInfraLine / FindInfraParallel / FindInfraCommonLine / ExtendInfraSegment return a List of unary InfraLine[{line}] wrappers; wrap with InfraLine @ ... to collapse via auto-flatten. Scene-language constructor InfraLine[p, q] is used inside InfraScene. Rendered like InfraSegment (sequential-edge semantics).";
FindInfraLine::usage = "FindInfraLine[graph, p1, p2] returns {InfraLine[{line}]} for one maximal geodesic extension through p1 and p2. FindInfraLine[graph, p1, p2, n] returns a List of n unary InfraLine[{line}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Options: \"Maximality\" (\"Extension\" (default) | \"Diameter\"), Method (\"ShortestPath\" (default), \"ShortestPathExtension\", \"CurvatureMinimizing\", \"Embedding\", \"Greedy\"), \"Pruning\" (caps prefix/suffix enumeration on \"ShortestPath\").";
FindInfraParallel::usage = "FindInfraParallel[graph, line, p] returns a List of unary InfraLine[{line}] wrappers for parallels through p (parallel = constant distance to line), or $Failed. FindInfraParallel[graph, line, p, n] / UpTo[n] / All controls multiplicity. Options: Method (\"ShortestPath\" (default) | \"Greedy\" | \"Embedding\"), \"Pruning\".";
FindInfraPerpendicular::usage = "FindInfraPerpendicular[graph, line, point] returns a List of unary InfraPoint[{foot}] wrappers for the feet of perpendiculars from point to line (Euclid I.12: isosceles base midpoint). FindInfraPerpendicular[graph, line, point, n] / UpTo[n] / All controls multiplicity. Option: Method (\"Metric\" (default) | \"Embedding\").";
FindInfraCommonLine::usage = "FindInfraCommonLine[graph, vertices] returns a List of unary InfraLine[{line}] wrappers for the canonical lines containing every listed vertex, or $Failed. With n / UpTo[n] / All controls multiplicity. Entries may be bare vertices or InfraPoint / InfraSegment / InfraLine / InfraRay wrappers.";
InfraSegmentLineAngle::usage = "InfraSegmentLineAngle[graph, p1, p2, line] (or InfraSegmentLineAngle[graph, segment, line]) measures the distance from a segment endpoint to a line, given the other endpoint lies on the line. The name is historical -- the value is a length, not a normalised angle.";
InfraLineQ::usage = "InfraLineQ[graph, segment] tests whether segment is a maximal geodesic.";
InfraParallelQ::usage = "InfraParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). InfraParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold.";
PencilDirections::usage = "PencilDirections[graph, O] returns the canonical maximal geodesics through O, one per projective direction class at O.";
PencilCardinality::usage = "PencilCardinality[graph, O] returns the number of distinct direction classes at O.";
LineCount::usage = "LineCount[graph] returns the number of distinct canonical maximal geodesics in graph.";

(* ===================== InfraShell ===================== *)

InfraShell::usage = "InfraShell[{set}] is the unary form (one metric shell vertex set); InfraShell[{set1, ..., setk}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraShell @ ... to collapse to multi via auto-flatten. Scene-language constructor InfraShell[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (induced-subgraph semantics).";
FindInfraShell::usage = "FindInfraShell[graph, c, r] returns {InfraShell[{levelSet}]} for the metric shell { v : d(c, v) == r }; r may be {rmin, rmax} for a tolerance band. FindInfraShell[graph, c, r, n] returns a List of n unary InfraShell[{set}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Options: Properties (list of \"Separating\" / \"Connected\"; AND-conjoined filter on candidate subsets; empty default returns the level set itself), Method (\"Exhaustive\" (default; BFS over the peel-DAG, accepts nested {\"Exhaustive\", \"Pruning\" -> spec}) | \"Greedy\" (DFS, one realisation); ignored when Properties is empty).";
FindInfraShellParameters::usage = "FindInfraShellParameters[graph, vertexSet] returns the list of {center, radius} pairs consistent with vertexSet being a metric shell.";
InfraShellQ::usage = "InfraShellQ[graph, vertexSet] tests whether vertexSet is a metric shell (equidistant level surface separating its interior from exterior). Use FindInfraShellParameters to recover {center, radius}.";
SeparatesQ::usage = "SeparatesQ[graph, vertexSet, u, v] tests whether deleting vertexSet disconnects u from v.";

(* ===================== InfraBall ===================== *)

InfraBall::usage = "InfraBall[{ball}] is the unary form (one closed metric ball vertex set); InfraBall[{ball1, ..., ballk}] is the multi-realisation form. Scene-language constructor InfraBall[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (induced-subgraph semantics).";
FindInfraBall::usage = "FindInfraBall[graph, c, r] returns InfraBall[{B_r(c)}] for the closed metric ball B_r(c) = { v : d(c, v) <= r }. Accepts InfraPoint[{{c}}] as the center for multi-anchor spread.";
InfraBallQ::usage = "InfraBallQ[graph, vertexSet] tests whether vertexSet is a closed metric ball, i.e. equals { v : d(c, v) <= r } for some center c in vertexSet and some radius r.";

(* ===================== InfraCircle ===================== *)

InfraCircle::usage = "InfraCircle[{cycle}] is the unary form; InfraCircle[{cycle1, ..., cyclek}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraCircle @ ... to collapse to multi via auto-flatten. Scene-language constructor InfraCircle[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (sequential edges with auto-closure).";
FindInfraCircle::usage = "FindInfraCircle[graph, c, r] returns {InfraCircle[{cycle}]} for the shortest simple cycle in the level-surface subgraph at radius r around c, as an open vertex sequence. FindInfraCircle[graph, c, r, n] returns a List of n unary InfraCircle[{cycle}] wrappers or $Failed (cycles sorted by length ascending); UpTo[n] returns up to n; All returns all. Options: Properties (currently \"Separating\" only; empty default permits any cycle in the level surface), Method (\"Exhaustive\" (default; FindCycle + filter + length sort, accepts nested {\"Exhaustive\", \"Pruning\" -> spec}) | \"Peel\" (BFS peel-DAG on the level-set vertices, accepts \"Pruning\") | \"Greedy\" (first admissible cycle by length, one realisation)).";
FindInfraCycle::usage = "FindInfraCycle[graph, n] returns n shortest simple cycles in graph as unary InfraCircle[{cycle}] wrappers (open vertex sequences, sorted by length ascending); UpTo[n] returns up to n; All returns all. FindInfraCycle[graph, {k}, n] restricts to cycles of exactly length k; FindInfraCycle[graph, {kMin, kMax}, n] to cycles in the length range. Returns $Failed when fewer than n cycles exist. Results feed directly into NullHomotopicQ and FindInfraNullHomotopy.";
InfraCircleQ::usage = "InfraCircleQ[graph, cycle] tests whether the vertex sequence cycle is a metric circle (cyclic edge chain whose vertex set is a metric shell). Accepts both open and closed input.";

(* ===================== InfraEllipticShell ===================== *)

InfraEllipticShell::usage = "InfraEllipticShell[{set}] is the unary form (one elliptic-shell vertex set); InfraEllipticShell[{set1, ..., setk}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraEllipticShell @ ... to collapse to multi via auto-flatten.";
FindInfraEllipticShell::usage = "FindInfraEllipticShell[graph, {p1, p2}, c] returns {InfraEllipticShell[{levelSet}]} for the elliptic shell { v : d(p1,v) + d(p2,v) == c }; c may be {cMin, cMax} for a tolerance band. FindInfraEllipticShell[graph, {p1, p2}, c, n] returns a List of n unary InfraEllipticShell[{set}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. Options: Properties (list of \"Separating\" / \"Connected\"; empty default returns the level set itself), Method (\"Exhaustive\" (default; BFS over the peel-DAG, accepts nested {\"Exhaustive\", \"Pruning\" -> spec}) | \"Greedy\" (DFS, one realisation); ignored when Properties is empty).";
InfraEllipticShellQ::usage = "InfraEllipticShellQ[graph, vertexSet] tests whether vertexSet is an elliptic shell, i.e. equals { v : d(p1,v) + d(p2,v) == c } for some p1, p2, c.";

(* ===================== InfraEllipse ===================== *)

InfraEllipse::usage = "InfraEllipse[{cycle}] is the unary form; InfraEllipse[{cycle1, ..., cyclek}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraEllipse @ ... to collapse to multi via auto-flatten.";
FindInfraEllipse::usage = "FindInfraEllipse[graph, {p1, p2}, c] returns {InfraEllipse[{cycle}]} for the shortest simple cycle in the level-surface subgraph { v : d(p1,v) + d(p2,v) == c }, as an open vertex sequence. FindInfraEllipse[graph, {p1, p2}, c, n] returns a List of n unary InfraEllipse[{cycle}] wrappers or $Failed (cycles sorted by length ascending); UpTo[n] returns up to n; All returns all. Options: Properties (currently \"Separating\" only; empty default permits any cycle in the level surface), Method (\"Exhaustive\" (default; FindCycle + filter + length sort, accepts nested {\"Exhaustive\", \"Pruning\" -> spec}) | \"Peel\" (BFS peel-DAG on the level-set vertices) | \"Greedy\" (first admissible cycle by length, one realisation)).";
InfraEllipseQ::usage = "InfraEllipseQ[graph, cycle] tests whether the vertex sequence cycle is a metric ellipse (cyclic edge chain whose vertex set is an elliptic shell). Accepts both open and closed input.";

(* ===================== InfraPlane ===================== *)

InfraPlane::usage = "InfraPlane[{set}] is the unary form (one bisecting hyperplane); InfraPlane[{set1, ..., setk}] is the multi-realisation form. Find* returns a List of unary wrappers; wrap with InfraPlane @ ... to collapse to multi via auto-flatten. Scene-language constructors InfraPlane[p1, p2] and InfraPlane[p1, p2, {lo, hi}] are used inside InfraScene.";
FindInfraBisectingHyperplane::usage = "FindInfraBisectingHyperplane[graph, p1, p2] returns {InfraPlane[{slab}]} for the bisector { v : d(p1, v) == d(p2, v) } (default Properties -> {}). FindInfraBisectingHyperplane[graph, p1, p2, n] returns a List of n unary InfraPlane[{set}] wrappers or $Failed; UpTo[n] returns up to n; All returns all. A positional {lo, hi} widens the bisector slab to lo <= d(p1, v) - d(p2, v) <= hi. Options: Properties (list of \"Separating\" / \"Connected\"; AND-conjoined filter on candidate subsets), Method (\"Exhaustive\" (default; BFS over the peel-DAG, accepts nested {\"Exhaustive\", \"Pruning\" -> spec}) | \"Greedy\" (DFS, one realisation)).";

(* ===================== InfraRay ===================== *)

InfraRay::usage = "InfraRay[{ray}] is the unary form (one pointed half-line from a base vertex O); InfraRay[{ray1, ..., rayk}] is the multi-realisation form. Each ray is a vertex sequence {O, ..., w} with w an inextensible endpoint. Find* returns a List of unary wrappers; wrap with InfraRay @ ... to collapse to multi via auto-flatten. Consumed by InfraSceneHighlight (sequential-edge semantics).";
FindInfraRay::usage = "FindInfraRay[graph, O, v] returns {InfraRay[{ray}]} for one pointed half-line from O in v's direction: the half of a maximal geodesic line through O and v starting at O and ending at the far inextensible endpoint. FindInfraRay[graph, O, v, n] returns a List of n unary InfraRay[{ray}] wrappers or $Failed; UpTo[n] / All controls multiplicity. O and v accept InfraPoint[{...}] for multi-anchor spread.";

(* ===================== InfraPolyline ===================== *)

InfraPolyline::usage = "InfraPolyline[{poly}] is the unary form: poly = {seg1, ..., segk} is a list of unary InfraSegment[{path_i}] with consecutive legs sharing their endpoint (Last[path_i] == First[path_{i+1}]). InfraPolyline[{poly1, ..., polym}] is the multi-realisation form. Consumed by InfraSceneHighlight (each realisation flattens to one concatenated vertex sequence; sequential-edge semantics).";
FindInfraPolylineSubdivision::usage = "FindInfraPolylineSubdivision[graph, path] returns {InfraPolyline[{{seg1, ..., segk}}]} where the legs are the fewest geodesic InfraSegments whose knots are path-vertices and each leg is a shortest path since the previous knot. Option: \"MaxLength\" (Infinity (default) | numeric L) caps every leg's graph-length at L.";
InfraPolylineQ::usage = "InfraPolylineQ[graph, poly] tests whether poly = {seg1, ..., segk} of unary InfraSegment wrappers is a valid polyline in graph: every leg is a geodesic and consecutive legs share an endpoint. Accepts the wrapped form InfraPolyline[{...}] as well (AllTrue over realisations).";

(* ===================== InfraRevolution ===================== *)

InfraObject::usage = "InfraObject[vs] wraps a vertex set vs as a single graph-geometric object; consumed by InfraSceneHighlight (induced-subgraph semantics).";
InfraRevolution::usage = "InfraRevolution[axis, profile] is the scene-language constructor for a rotational object inside InfraScene.";
FindInfraRevolution::usage = "FindInfraRevolution[graph, axis, profile] returns InfraObject[set] for the rotational vertex set around axis with the given radius profile. The axis is internally extended by +1 position on each side via ExtendInfraSegment[..., \"Length\" -> 1] so the orthogonality test sees a one-step extension at each endpoint. The profile may be NumericQ (constant), a List of length |axis|, an Association vertex -> radius, or any callable applied to index 1..|axis|. Options: \"Form\" (\"Solid\" (default) | \"Surface\"); Method (\"Voronoi\" (default) -- closest extended position must be in the original axis range; \"PerpendicularBisector\" -- u included at position i iff d(u, v_{i-1}) == d(u, v_{i+1})).";
FindInfraCylinder::usage = "FindInfraCylinder[graph, axis, r] returns InfraObject[set] for the constant-radius rotational set around axis. Inherits FindInfraRevolution options.";
FindInfraCone::usage = "FindInfraCone[graph, axis, slope] returns InfraObject[set] for the cone of given slope with apex at axis[[1]]: radii are slope * Range[0, Length[axis] - 1]. Option \"Apex\" (First (default) | Last) flips the apex end; inherits remaining options from FindInfraRevolution.";
InfraRevolutionQ::usage = "InfraRevolutionQ[graph, vs, axis, profile] tests whether vs equals the rotational vertex set around axis with the given radius profile. Option \"Form\" matches FindInfraRevolution.";

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

SelectInfraPath::usage = "SelectInfraPath[graph, paths] returns one path drawn from the bundle treated as a finite metric space in path-space. SelectInfraPath[graph, paths, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole pool. Options mirror FindInfraPoint: \"From\" (All (default), \"Center\", \"Periphery\", \"MostVisited\", anchor -> spec, InfraSegment[{...}] -> spec, \"MinCurvature\" / \"MaxCurvature\" with optional nested {curv, agg} where curv is \"FormanRicciCurvature\" | \"WolframRicciCurvature\" | \"OllivierRicciCurvature\" and agg is \"Mean\" (default) | \"Total\" | \"Max\"), \"Distance\" (None (default), \"Max\", d, {dMin, dMax}), \"Metric\" (\"Hausdorff\" (default), \"Frechet\", \"MeanFrechet\"), \"MaxCliques\". Accepts InfraSegment and InfraRay wrappers; preserves the wrapper. Operator form SelectInfraPath[graph, n, opts][paths].";
SelectInfraCycle::usage = "SelectInfraCycle[graph, cycles] returns one cycle drawn from the bundle in path-space. SelectInfraCycle[graph, cycles, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole pool. Options mirror SelectInfraPath, with two extra \"From\" values: \"ShortestCircumference\", \"LongestCircumference\". Path-space metric distances factor cyclic rotation so they are rotation-invariant. Accepts InfraCircle[cycles_List]; preserves the wrapper. Operator form SelectInfraCycle[graph, n, opts][cycles].";
EmbeddingClosest::usage = "EmbeddingClosest[graph, bundle, {p1, p2}] keeps the bundle elements whose drawing under GraphEmbedding is Hausdorff-closest to the straight Euclidean segment p1-p2. EmbeddingClosest[graph, bundle, {center, radius}] keeps the bundle elements Hausdorff-closest to the Euclidean circle of given centre and radius. Dispatch is by reference shape: {vertex, vertex} for paths, {vertex, numeric} for cycles. Accepts bare lists, InfraSegment / InfraLine / InfraPath / InfraRay / InfraCircle wrappers, or homogeneous lists of unary wrappers; wrappers are preserved. Operator form EmbeddingClosest[graph, ref][bundle].";
GeodesicGraph::usage = "GeodesicGraph[graph, c] returns the BFS DAG rooted at c: a directed graph with edge u -> v whenever d(c, v) = d(c, u) + 1 and u-v is an edge of graph. Sinks are peripheral vertices reachable from c; directed paths c -> sink are exactly the maximal geodesics from c. Accepts InfraPoint[{c1, ..., ck}] for multi-source BFS. Option: \"AxisLength\" (truncate at depth k; default All).";
GeodesicSubgraph::usage = "GeodesicSubgraph[graph, pairs] returns the union of geodesics between the listed vertex pairs. Options: \"PathThickness\" (Hausdorff threshold for keeping multiple geodesics per pair), \"Directed\".";
PathSubgraph::usage = "PathSubgraph[graph, u, v] returns the union of all shortest u-v paths. PathSubgraph[graph, u, v, k] (or UpTo[k]) caps path length; PathSubgraph[graph, u, v, All] returns the full simple-path subgraph. Option: \"Directed\".";
InfraPathLength::usage = "InfraPathLength[w] returns the edge count of a path-type wrapper w (InfraSegment, InfraPath, InfraLine, InfraRay). For InfraCircle, returns the circumference (vertex count of the open cycle). For InfraPolyline, returns the sum of leg edge counts. Multi-realisation wrappers return a List of lengths.";

(* ===================== Homotopy ===================== *)

InfraHomotopy::usage = "InfraHomotopy[{chain}] is the unary form: one homotopy chain {p0, p1, ..., pk} of intermediate walks produced by k elementary moves from p0 to pk. InfraHomotopy[{chain1, ..., chaink}] is the multi-realisation form. Find* finders return a List of unary InfraHomotopy[{chain}] wrappers; wrap that list with InfraHomotopy @ ... to collapse to multi via auto-flatten.";
FindInfraHomotopy::usage = "FindInfraHomotopy[graph, p1, p2] returns {InfraHomotopy[{chain}]} exhibiting a chain of arc-swap moves taking walk p1 to walk p2. FindInfraHomotopy[graph, p1, p2, n] returns a List of n wrappers; UpTo[n] returns up to n; All returns all. Endpoints accept InfraSegment[{...}] for multi-anchor spread. Option Method: \"ViaMinimalForm\" (default; reduce both endpoints to a common minimal form and concat) or \"MinimumMoves\" (BFS for the chain with fewest elementary moves). Option \"NullHomotopicCycles\": integer k, list of lengths, or list of vertex cycles; integer k is shorthand for Range[1, k]; length-1 is consecutive-duplicate {a, a}, length-2 is the 2-cycle a-b-a, length k >= 3 uses the fundamental cycle basis; default {1, 2, 3}. Options \"MaxLength\" (walk-length cap during BFS; Automatic computes a generous default) and \"MaxMoves\" (BFS depth cap; default Infinity).";
FindInfraNullHomotopy::usage = "FindInfraNullHomotopy[graph, cycle] returns {InfraHomotopy[{chain}]} exhibiting a chain contracting cycle to its base vertex, or {} if cycle is not null-homotopic. With n / UpTo[n] / All controls multiplicity. Accepts InfraCircle[{...}] for multi-cycle spread. Inherits FindInfraHomotopy options.";
FindInfraMinimalForms::usage = "FindInfraMinimalForms[graph, p] returns one length-shortest walk in the homotopy class of p. FindInfraMinimalForms[graph, p, n] returns n minimal forms; UpTo[n] / All control multiplicity. Uses bidirectional BFS bounded by \"MaxLength\" -- a reduction may need to go up before coming down. Options \"NullHomotopicCycles\" (default {1, 2, 3}), \"MaxLength\" (Automatic), \"MaxMoves\" (Infinity).";
FindInfraReduction::usage = "FindInfraReduction[graph, p] returns one reduction chain {p, ..., m} of elementary moves ending at a minimal form m of p. FindInfraReduction[graph, p, n] returns n chains; UpTo[n] / All control multiplicity. Same options as FindInfraMinimalForms.";
HomotopicQ::usage = "HomotopicQ[graph, p1, p2] tests whether walks p1 and p2 are homotopic, equivalent to whether their minimal-form sets intersect. Spreads over multi-realisation inputs as a Cartesian-AllTrue conjunction. Inherits FindInfraHomotopy options.";
NullHomotopicQ::usage = "NullHomotopicQ[graph, cycle] tests whether cycle is null-homotopic (contractible to a constant walk via arc-swap moves). Inherits FindInfraHomotopy options.";
ReducePath::usage = "ReducePath[graph, path] returns one minimal form of path -- a length-shortest walk in its homotopy class. Convenience wrapper around FindInfraMinimalForms[graph, path, 1]. Options \"NullHomotopicCycles\" (default {1, 2, 3}), \"MaxLength\" (Automatic), \"MaxMoves\" (Infinity).";
HomotopyMoveType::usage = "HomotopyMoveType[walk1, walk2] classifies the elementary move walk1 -> walk2 as \"Contract\" (walk shortens), \"Extend\" (walk lengthens), or \"Lateral\" (walks have the same length; only possible for moves that swap arcs of equal length on an even-length null-homotopic cycle).";
HomotopyMoveTypes::usage = "HomotopyMoveTypes[chain] applies HomotopyMoveType to each consecutive pair in chain. HomotopyMoveTypes[InfraHomotopy[{chain}]] returns the labels for the unary wrapper; HomotopyMoveTypes[InfraHomotopy[reps]] returns one label list per realisation.";
HomotopicLoopsQ::usage = "HomotopicLoopsQ[graph, loop1, loop2] tests free loop homotopy: loop1 and loop2 are equivalent under path-homotopy moves and cyclic rotation of the loop (the user-framework definition: homotopy of paths going through loops and rotations). Two loops with disjoint vertex sets return False -- the through-loops definition can't bridge them without an external connecting path. Open input is auto-closed; accepts InfraCircle[{...}] on either side as a Cartesian-AllTrue conjunction. Inherits FindInfraHomotopy options.";

(* ===================== MetricAlgebra ===================== *)

MetricInterval::usage = "MetricInterval[graph, u, v] returns the vertex set { w : d(u, w) + d(w, v) == d(u, v) } -- the union of all geodesics from u to v.";
GeodesicMultiplicity::usage = "GeodesicMultiplicity[graph, u, v] returns the number of distinct geodesics from u to v, computed as (A^d)[u, v] where d = GraphDistance[graph, u, v].";
GeodesicMultiplicityMatrix::usage = "GeodesicMultiplicityMatrix[graph] returns {D, M} where D is the distance matrix and M[i, j] is the number of geodesics from vertex i to vertex j.";
MedianVertices::usage = "MedianVertices[graph, vs] returns the vertices minimising the sum of distances to vs. A graph is a median graph iff every triple has a unique median.";
FindGeodesicConvexHull::usage = "FindGeodesicConvexHull[graph, S] returns the smallest superset of S closed under MetricInterval, as a sorted vertex list. Graph-intrinsic shadow of tropical convexity (see Wiki/Concepts/TropicalConvexity).";
GeodesicallyConvexQ::usage = "GeodesicallyConvexQ[graph, S] tests geodesic convexity of S. Option Method (\"Strong\" (default): every geodesic between any pair of S lies in S; \"Weak\": some geodesic between each pair lies in S).";

(* ===================== InfraTopology ===================== *)

BallTopologyGraph::usage = "BallTopologyGraph[graph, r] returns the directed graph of the specialization preorder of the Alexandrov topology on V(graph) whose closed-set subbasis is the family of closed r-balls (edge q -> p iff N_r(p) is contained in N_r(q)). Option \"Reduced\" (True (default): Hasse-style transitive reduction, self-loops dropped; False: full preorder digraph with reflexive self-loops).";
BallClosure::usage = "BallClosure[graph, r, p] returns the closure of vertex p in the Alexandrov topology with closed-set subbasis the closed r-balls, as a vertex List.";
BallContinuousMapQ::usage = "BallContinuousMapQ[g, r, h, s, map] tests whether map: V(g) -> V(h) is continuous for the r-ball topology on g and the s-ball topology on h, i.e. monotone for the specialization preorder. map: Association, list of Rule, or callable.";

(* ===================== Coordinatization ===================== *)

ResistanceCoordinates::usage = "ResistanceCoordinates[graph] returns the Association v -> Phi(v) of resistance-matching spectral coordinates Phi(v) = (phi_i(v) / Sqrt[lambda_i])_{i: lambda_i > 0}, satisfying ||Phi(u) - Phi(v)||^2 == EffectiveResistance(u, v). ResistanceCoordinates[graph, v] returns one coordinate vector; an InfraPoint query returns the per-realisation list. Options: \"Rescaling\" (\"ResistanceMatching\" (default), \"None\", \"Diffusion\" -> t), \"Dimension\" (Automatic, Integer, UpTo[k], All), \"Origin\" (None or vertex / InfraPoint).";

FindInfraRadarBasis::usage = "FindInfraRadarBasis[graph, n, m] returns up to n radar bases (resolving sets); m specifies basis sizes (All, an integer, {min, max}, or {exact}).";
InfraRadarBasisQ::usage = "InfraRadarBasisQ[graph, basis] tests whether basis is a radar basis (every vertex has a unique distance vector to the basis).";
RadarCoordinates::usage = "RadarCoordinates[graph, basis, vertex] returns the radar coordinates of vertex (distance vector to basis). RadarCoordinates[graph, basis] returns the Association for all vertices, so RadarCoordinates[graph, basis][vertex] is the natural operator form. Accepts InfraPoint[{...}] as the query point (singleton degenerates, multi-vertex returns the list of per-realisation vectors) and as basis entries (aggregated by option \"InfraPointAggregation\" -> Min (default), Mean, or Max).";
OrthogonalCoordinates::usage = "OrthogonalCoordinates[graph, c, {a1, a2, ...}, v] returns the Z-valued displacement of v on each axis ai through the centre c; OrthogonalCoordinates[graph, c, {a1, a2, ...}] returns the Association for all vertices. Centre c is a vertex or InfraPoint; each axis ai is an InfraSegment, InfraLine, or vertex sequence. Option \"SelectCoordinate\" (projection-tie reducer applied to the shifted tied list ix_v - k: \"Centered\" (default; 0 if 0 in shifted, else Round[Median]), Min, Max, Mean, Median, All, or any user function).";
FindInfraOrthogonalFrame::usage = "FindInfraOrthogonalFrame[graph, c, axisLength] returns one orthogonal frame at centre c with each axis half-depth constrained by axisLength (All | n_Integer | UpTo[n] | {min, max}): a list of InfraSegment axes (full metric lines through c, c strictly interior), mutually perpendicular at c. FindInfraOrthogonalFrame[graph, c, axisLength, n] returns exactly n distinct frames or $Failed; UpTo[n] returns up to n; All returns every frame. Centre is a vertex or InfraPoint[{c1, ..., ck}]. Options: Method (Automatic = \"Exhaustive\", or \"Greedy\"), \"AxisCount\" (Automatic | k | UpTo[k] | All), \"BranchSampleSize\" (Exhaustive subsample cap), \"SelectCoordinate\" (projection-tie reducer; perpendicularity at c is defined as the per-axis coord being 0 under this reducer applied to the shifted tied list ix_w - k; \"Centered\" (default; 0 if 0 in shifted, else Round[Median]), Min, Max, Mean, Median, All, ...). For the no-centre form use FindInfraSpanningAxes.";
FindInfraSpanningAxes::usage = "FindInfraSpanningAxes[graph, n] returns n mutually well-separated longest geodesics across graph (greedy, no fixed center) or $Failed; UpTo[n] returns up to n; All returns every axis above the separation threshold. Options: \"AxisDistance\" (\"MinEndpoint\" | \"Hausdorff\" | \"Separation\"), \"MinLength\", \"MinSeparation\", \"AxisThickness\", \"RandomPick\". For axes through a fixed center vertex use FindInfraOrthogonalFrame.";

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
InfraPathQ::usage = "InfraPathQ[w] asserts that w is a valid simple path (consecutive adjacency, no repeated vertices).";
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
$InfraBallColor::usage    = "Default highlight color for InfraBall objects.";
$InfraPlaneColor::usage   = "Default highlight color for InfraPlane objects.";
$InfraCircleColor::usage  = "Default highlight color for InfraCircle objects.";
$InfraRayColor::usage     = "Default highlight color for InfraRay objects.";
$InfraObjectColor::usage  = "Default highlight color for InfraObject objects.";

InfraSceneHighlight::usage = "InfraSceneHighlight[g, multiObjects] renders a list of multi-objects diffusely on graph g, with intensity scaling by overlap within each object and color-blending across objects. Each entry is auto-classified by representation; explicit Infra* wrappers force the intended semantics, and `entry -> color` overrides the default per-head colour. Options: \"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\".";
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] is an interactive visualisation of an InfraScene on a graph; an optional third Association of pre-fixed bindings is supported. Controls: step slider, \"Fix & advance\", \"Reset\".";
PointViewer::usage = "PointViewer[g] is an interactive viewer for selecting points in graph g. PointViewer[g, sym] stores the current selection in sym.";
SegmentViewer::usage = "SegmentViewer[g] is an interactive viewer for exploring geodesic segments in graph g.";
ShellViewer::usage = "ShellViewer[g] is an interactive viewer for exploring metric shells in graph g. Method setter: \"Metric\" | \"Separating\".";
CircleViewer::usage = "CircleViewer[g] is an interactive viewer for exploring separating cycles in graph g.";
