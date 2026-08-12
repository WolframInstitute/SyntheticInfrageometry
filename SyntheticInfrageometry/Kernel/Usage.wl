Package["WolframInstitute`SyntheticInfrageometry`"]

(* Usage messages: one sentence per signature; options listed by name only.
   Full method/sub-option trees and worked examples live in the guides
   and tutorials, not here. See CLAUDE.md "Usage messages" for the rules. *)

(* ===================== InfraPoint ===================== *)

InfraPoint::usage = "InfraPoint[{v1, ..., vk}] is the multi-realisation point -- a candidate set in superposition. InfraPoint[<|v -> m, ...|>] is the measured form: the measure on vertices, stored as the association it is (InfraPoint is the only measured wrapper, since its realisations are vertices and so its vertex marginal is lossless). InfraPoint[{v1, ...}, {m1, ...}] is input sugar for the association; repeated vertices sum and an all-ones measure collapses to the bare support. Measured points are constructed at projections -- seg[[i]], FindInfraMidpoint, FindInfraShellCenter. Accessors [\"Support\"], [\"Weights\"], [\"Mass\"], [\"First\"], plus graph-keyed synthetic invariants ([\"BallVolumes\", g], [\"Dimension\", g], ...) which read the support only. Scene-language constructors InfraPoint[] / InfraPoint[\"Center\"] / InfraPoint[origin, dist] are used inside InfraScene.";
FindInfraPoint::usage = "FindInfraPoint[graph] returns THE canonical point: the whole candidate pool in superposition, InfraPoint[pool]. FindInfraPoint[graph, n] returns a tuple of n mutually-constrained points as a List of unary wrappers, exactly n or $Failed; UpTo[n] returns up to n; All returns every vertex. Options \"From\" (\"Random\" (default), \"Center\", \"Periphery\", anchor -> spec), \"Distance\" (d, {dMin, dMax}, \"Max\", \"Spread\"), \"MaxCliques\".";
FindInfraMidpoint::usage = "FindInfraMidpoint[graph, p1, p2] returns one InfraPoint of the walk-centre vertices unioned over every geodesic (one vertex at even distance, a two-vertex mesopoint at odd). FindInfraMidpoint[graph, InfraSegment[{walks}]] and FindInfraMidpoint[graph, walk] use the supplied walks. Options Method (\"Metric\" (default), \"Embedding\"), \"Tolerance\".";
FindInfraGoldenSection::usage = "FindInfraGoldenSection[graph, p1, p2] returns one InfraPoint at the golden index 1 + (n-1)/GoldenRatio per geodesic, unioned. FindInfraGoldenSection[graph, InfraSegment[{walks}]] and FindInfraGoldenSection[graph, walk] use the supplied walks. Options Method (\"Metric\" (default), \"Embedding\"), \"Tolerance\".";
FindInfraReflection::usage = "FindInfraReflection[graph, x, a] returns InfraPoint[{x1', x2', ...}], the reflections of x through a in superposition, where each x' satisfies B(x, a, x') and ax == ax'. FindInfraReflection[graph, x, a, n] returns exactly n realisations or $Failed; UpTo[n] up to n; All (default) all.";
CompleteInfraEquilateralTriangle::usage = "CompleteInfraEquilateralTriangle[graph, p1, p2] returns InfraPoint[{c1, c2, ...}], the apex vertices equidistant from p1 and p2 at distance d(p1, p2) (Euclid I.1). The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed. Option: Method (\"Metric\" (default)).";
FindInfraCommonPoint::usage = "FindInfraCommonPoint[graph, lines] returns InfraPoint[{v1, v2, ...}] of the vertices on every listed line. The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed. Entries may be bare vertex sequences or InfraSegment / InfraRay wrappers.";
FindClosestInfraPoint::usage = "FindClosestInfraPoint[graph, line, point] returns unary InfraPoint[{v}] wrappers for the vertices on line at minimum graph distance from point; n / UpTo[n] / All controls multiplicity. line is a vertex sequence or path-shaped wrapper; point a vertex or InfraPoint.";
SelectInfraPoint::usage = "SelectInfraPoint[graph, vertices] draws one vertex from the bundle under graph distance. SelectInfraPoint[graph, vertices, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole filtered pool. Options mirror FindInfraPoint (\"From\", \"Distance\", \"MaxCliques\"); the bundle may be a vertex list or any set-like Infra* wrapper. Operator form SelectInfraPoint[graph, n, opts][vertices].";
InfraReachableQ::usage = "InfraReachableQ[graph, p1, p2] tests whether some realisation of p1 lies in the same connected component as some realisation of p2. Accepts InfraPoint[{...}], a bare vertex, or a list of vertices.";

(* ===================== InfraSegment ===================== *)

InfraSegment::usage = "InfraSegment[{path1, ..., pathk}] is the multi-realisation geodesic bundle -- a set of alternative geodesics, carrying no masses; seg[[i]] is the measured InfraPoint of the i-th position across realisations (mass = multiplicity), which is where a measure gets constructed. InfraSegment[dag_Graph] is the geodesic-DAG form with accessors [\"Graph\"], [\"Vertices\"], [\"Length\"], [\"Multiplicity\"], [\"Start\"] / [\"End\"], [\"Measure\"], and [\"Realizations\", n | UpTo[n] | All] to enumerate. Scene-language constructor InfraSegment[p, q] is used inside InfraScene.";
FindInfraSegment::usage = "FindInfraSegment[graph, p1, p2] returns the compact canonical form: InfraSegment[dag] (the geodesic interval DAG of all shortest paths), or a weighted bundle of per-pair DAG atoms for weighted / multi InfraPoint endpoints. FindInfraSegment[graph, p1, p2, n] returns one InfraSegment with exactly n enumerated paths or $Failed; UpTo[n] up to n; All the fully enumerated bundle. Options Properties ({} (default); {\"EdgeMin\", f}, {\"EdgeMax\", f}, \"LongestPath\"), Method (\"Exhaustive\" (default), \"Greedy\").";
ExtendInfraSegment::usage = "ExtendInfraSegment[graph, a, b, c, d] returns InfraPoint[{x}] for x with B(a, b, x) and d(b, x) == d(c, d) (Tarski axiom A4); n / UpTo[n] / All controls multiplicity.";
InfraPathQ::usage = "InfraPathQ[graph, walk] tests whether walk is a simple path in graph (consecutive adjacency and no repeated vertices). Hierarchy: InfraPathQ \[Superset] InfraSegmentQ \[Superset] InfraLineQ.";
InfraPath::usage = "InfraPath[{walk}] is the unary form (one walk, possibly non-simple); InfraPath[{walk1, ..., walkk}] is the multi-realisation form. Inside InfraScene, InfraPath[p1, p2, ..., pk] is the step-wise scene-DSL constructor: every length-(k-1) walk {v1, ..., vk} with vi in pi and each consecutive pair (vi, v_{i+1}) a graph edge (non-simple chains kept). FindInfraPath finders return one InfraPath wrapper carrying the requested realisations. Rendered like InfraSegment (sequential-edge semantics).";
FindInfraPath::usage = "FindInfraPath[graph, p1, p2, kspec] returns one InfraPath wrapper of walks p1 -> p2 (non-simple allowed) with length restricted by kspec (k | {k} | {kmin, kmax} | Infinity); trailing count n returns exactly n realisations or $Failed, UpTo[n] up to n, All (default) all. Options Properties (\"Simple\", \"ShortestPath\", \"LongestPath\", {\"EdgeMin\", f}, {\"EdgeMax\", f}), Method (\"Exhaustive\").";
ExtendInfraPath::usage = "ExtendInfraPath[graph, path] extends a walk per-step until inextensible and returns unary InfraPath[{walk}] wrappers; n / UpTo[n] / All controls multiplicity. Options Properties (as FindInfraPath), Method, \"Length\" (step budget), \"Direction\" (\"BothSides\" (default), \"Forward\", \"Backward\").";
ConcatenateInfraPath::usage = "ConcatenateInfraPath[path1, path2] joins every compatible walk pair (Last[walk1] === First[walk2]) as unary InfraPath wrappers; n / UpTo[n] / All controls multiplicity (default All).";
InfraLoop::usage = "InfraLoop[{walk}] is the unary form (one closed walk, First === Last); InfraLoop[{walk1, ..., walkk}] is the multi-realisation form. Open-walk realisations are auto-closed by appending First. Inside InfraScene, InfraLoop[v1, v2, ..., vk] is the scene-DSL constructor (auto-closes if needed). Used by the polymorphic homotopy finders as the base-pointed loop wrapper (homotopy fixes the base vertex unless \"FreeHomotopy\" -> True).";
InfraString::usage = "InfraString[{walk}] is the unary form (one closed walk modulo cyclic rotation); InfraString[{walk1, ..., walkk}] is the multi-realisation form. Each realisation is stored as the lex-least cyclic rotation of Most[closeWalk[walk]] (the open canonical form, without the wrap-around vertex repetition); orientation is preserved. Equality between strings is SameQ on canonical forms. Inside InfraScene, InfraString[v1, v2, ..., vk] is the scene-DSL constructor (auto-closes and canonicalises). Used by the polymorphic homotopy finders as the free-loop wrapper (no base point; cyclic rotations are identified).";
InfraSegmentQ::usage = "InfraSegmentQ[graph, segment] tests whether segment is a geodesic path.";
UniqueInfraSegmentQ::usage = "UniqueInfraSegmentQ[graph, u, v] tests whether the geodesic from u to v is unique. UniqueInfraSegmentQ[graph] tests the geodetic property (every pair has a unique geodesic).";

(* ===================== InfraLine ===================== *)

InfraLine::usage = "InfraLine[{line}] is the unary form (one maximal geodesic); InfraLine[{line1, ..., linek}] is the multi-realisation form. Single-index line[[i]] returns the weighted InfraPoint of the i-th position across realisations (mass = multiplicity; i may be negative); First[line] and multi-index line[[1,1]] keep the realisation-list / first-line meaning. FindInfraLine / FindInfraParallel / FindInfraCommonLine return one InfraLine wrapper carrying the requested realisations. Scene-language constructor InfraLine[p, q] is used inside InfraScene. Rendered like InfraSegment (sequential-edge semantics).";
FindInfraLine::usage = "FindInfraLine[graph, p1, p2] returns one InfraLine wrapper of the maximal geodesic lines through p1 and p2; FindInfraLine[graph, segment] returns maximal lines containing segment; count n returns exactly n realisations or $Failed, UpTo[n] up to n, All (default) all. Options Properties ({}), Method (\"Exhaustive\" (default), \"Greedy\"), \"Maximality\" (\"Extension\" (default), \"Diameter\"), \"Direction\" (\"BothSides\" (default), \"Forward\", \"Backward\").";
FindInfraParallel::usage = "FindInfraParallel[graph, line, p] returns unary InfraLine[{line}] wrappers for parallels through p (constant distance to line); n / UpTo[n] / All controls multiplicity. Options Properties, Method.";
FindInfraPerpendicular::usage = "FindInfraPerpendicular[graph, line, point] returns InfraLine realisations through point perpendicular to line; n / UpTo[n] / All controls multiplicity. Options Method (\"Metric\" (default; Euclid I.12 foot), \"Projection\", \"Coordinate\", \"Arclength\", \"Alexandrov\"; sub-options inside the spec forward to InfraPerpendicularQ), \"Radius\".";
FindInfraCommonLine::usage = "FindInfraCommonLine[graph, vertices] returns InfraLine[{line1, ...}] of the canonical lines containing every listed vertex. The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed. Entries may be bare vertices or InfraPoint / InfraSegment / InfraLine / InfraRay wrappers.";
InfraLineQ::usage = "InfraLineQ[graph, segment] tests whether segment is a maximal geodesic.";
InfraParallelQ::usage = "InfraParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). InfraParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold.";
InfraPerpendicularQ::usage = "InfraPerpendicularQ[graph, l1, l2] tests whether two lines are perpendicular at every common vertex (False on empty intersection); inside InfraScene the graph-free InfraPerpendicularQ[l1, l2] asserts the same. Options Method (\"Projection\" (default), \"Coordinate\", \"Arclength\", \"Alexandrov\"; per-method sub-options live inside the spec), \"Radius\".";
PencilDirections::usage = "PencilDirections[graph, O] returns the canonical maximal geodesics through O, one per projective direction class at O.";
PencilCardinality::usage = "PencilCardinality[graph, O] returns the number of distinct direction classes at O.";
LineCount::usage = "LineCount[graph] returns the number of distinct canonical maximal geodesics in graph.";
FindLineHull::usage = "FindLineHull[graph, S] returns the smallest superset of S closed under the line operator (the maximal geodesics through each pair), as an InfraSet. S is any Infra* object, a list of them, or a bare vertex list. Option \"LineStructure\" (None (default), or an InfraLineStructure / list of lines) closes under that fixed line family instead of all maximal geodesics.";
LineHullQ::usage = "LineHullQ[graph, S] returns True if S is closed under the line operator, i.e. equals its own FindLineHull. Option \"LineStructure\" as in FindLineHull.";
UniversalLineQ::usage = "UniversalLineQ[graph] returns True if some pair of vertices spans a line filling a whole connected component (Chen-Chvatal). UniversalLineQ[graph, {u, v}] tests the single line through u, v.";

(* ===================== InfraLineStructure ===================== *)

InfraLineStructure::usage = "InfraLineStructure[{line1, ..., linek}] is a consistent geodesic path system, stored as its maximal lines (the structure's realisations). Accessors: [\"Lines\"] / [\"Realizations\"] the maximal lines, [\"First\"] / [[i]] one line, [\"Length\"] per-line edge counts; [\"Paths\"] the unfolded association <|{u,v} -> P(u,v)|>, [\"Incidence\"] the transpose <|v -> {line numbers}|>, [\"Coordinates\"] <|v -> {{line, offset}}|> (offset = edges from the line start), and [\"Path\", u, v] recovers P(u,v) oriented u -> v.";

FindLineStructure::usage = "FindLineStructure[graph] returns an InfraLineStructure[{lines}]: a consistent (subpath-closed) geodesic path system -- one shortest path per pair, with every stretch of a chosen path again the chosen path -- stored as its maximal lines. Option Method (\"Lexicographic\" (default; exact 1 + 2^(-rank) weighting, edges ranked by sorted endpoints) | {\"Random\", seed} | \"Resistance\" | \"Weight\" -> w) supplies the edge ranking that breaks geodesic ties; every method yields a consistent system. See InfraLineStructure for accessors.";

ConsistentPathSystemQ::usage = "ConsistentPathSystemQ[graph, obj] tests whether a geodesic path system is subpath-closed (Cizma-Linial consistent). obj is an InfraLineStructure, a list of lines, or an association <|{u,v} -> path|>.";

(* ===================== InfraShell ===================== *)

InfraShell::usage = "InfraShell[{set}] is the unary form (one metric shell vertex set); InfraShell[{set1, ..., setk}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations. Scene-language constructor InfraShell[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (induced-subgraph semantics).";
FindInfraShell::usage = "FindInfraShell[graph, c, r] returns one InfraShell wrapper for the metric shell { v : d(c, v) == r } (r may be {rmin, rmax}). FindInfraShell[graph, c, r, n] returns exactly n realisations or $Failed; UpTo[n] up to n; All (default) all. Options Properties (\"Separating\", \"Connected\"), Method (\"Exhaustive\" (default), \"Greedy\").";
FindInfraOsculatingShell::usage = "FindInfraOsculatingShell[graph, path, i, k] returns shells whose level set contains the k-vertex window centered on path[[i]], one per osculating center, sorted by ascending radius; n / UpTo[n] / All controls multiplicity. Forwards Properties / Method to FindInfraShell.";
FindAdvancingInfraFront::usage = "FindAdvancingInfraFront[graph, origin, steps] returns the foliation { InfraSet[S_0], ..., InfraSet[S_steps] } of a bouncing wavefront: each front vertex steps one geodesic step outward from the previous front and reflects inward where there is no outward neighbour, so the front never empties. origin is a vertex or InfraPoint multi-source.";
FindInfraShellCenter::usage = "FindInfraShellCenter[graph, shell] returns {center, radii}: a weighted InfraPoint of centers and a sorted radius list. Option Method (\"MaximalChordsBisectors\" (default; sub-options \"Maximality\", \"Distance\", \"Parity\") | \"EquidistantPoints\").";
InfraShellQ::usage = "InfraShellQ[graph, vertexSet] tests whether vertexSet is a metric shell, i.e. some vertex c is equidistant from all of vertexSet at a common radius r and vertexSet equals the level set { v : d(c, v) == r }.";
SeparatesQ::usage = "SeparatesQ[graph, vertexSet, u, v] tests whether deleting vertexSet disconnects u from v.";

(* ===================== InfraBall ===================== *)

InfraBall::usage = "InfraBall[{ball}] is the unary form (one closed metric ball vertex set); InfraBall[{ball1, ..., ballk}] is the multi-realisation form. Scene-language constructor InfraBall[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (induced-subgraph semantics).";
FindInfraBall::usage = "FindInfraBall[graph, c, r] returns InfraBall[{B_r(c)}] for the closed metric ball B_r(c) = { v : d(c, v) <= r }. A multi-anchor center (InfraPoint wrapper, possibly weighted) spreads into one realisation per center carrying the center's mass.";
InfraBallQ::usage = "InfraBallQ[graph, vertexSet] tests whether vertexSet is a closed metric ball, i.e. equals { v : d(c, v) <= r } for some center c in vertexSet and some radius r.";
FindBallHull::usage = "FindBallHull[graph, S] returns the ball hull of S as an InfraSet: the intersection of all closed balls containing S, the smallest ball-convex (Mazur) superset. S is any Infra* object, a list of them, or a bare vertex list.";
BallHullQ::usage = "BallHullQ[graph, S] returns True if S is ball-convex, i.e. equals its own ball hull (an intersection of balls).";

(* ===================== InfraCircle ===================== *)

InfraCircle::usage = "InfraCircle[{cycle}] is the unary form; InfraCircle[{cycle1, ..., cyclek}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations. Scene-language constructor InfraCircle[center, radius] is used inside InfraScene. The multi form is consumed by InfraSceneHighlight (sequential edges with auto-closure).";
FindInfraCircle::usage = "FindInfraCircle[graph, c, r] returns one InfraCircle wrapper for the canonical infra-circle around c at radius r: the shortest separating cycle in the level surface and its ties (r scalar or {rmin, rmax}). FindInfraCircle[graph, c, r, n] returns exactly n realisations or $Failed; UpTo[n] up to n; All (default) all. Option Properties (default {\"Separating\", \"Shortest\"}).";
FindInfraCycle::usage = "FindInfraCycle[graph, n] returns n shortest simple cycles as unary InfraCircle[{cycle}] wrappers (sorted by length); UpTo[n] returns up to n; All returns all. FindInfraCycle[graph, {k}, n] and FindInfraCycle[graph, {kMin, kMax}, n] restrict cycle length.";
InfraCircleQ::usage = "InfraCircleQ[graph, cycle] tests whether the vertex sequence cycle is a metric circle (cyclic edge chain whose vertex set is a metric shell). Accepts both open and closed input.";

(* ===================== InfraPolygon ===================== *)

InfraPolygon::usage = "InfraPolygon[{poly}] is the unary form, where poly = {seg1, ..., segn} is a closed chain of unary InfraSegment sides; InfraPolygon[{poly1, ..., polyk}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations. Accessors [\"Sides\"] (the InfraSegment legs per realisation), [\"Length\"] (perimeter edge count per realisation), [\"Vertices\"] (corner vertices as unary InfraPoints).";
FindInfraPolygon::usage = "FindInfraPolygon[graph, {p1, ..., pn}] returns {InfraPolygon[{poly}]} for the polygon through corners p1, ..., pn (n >= 3), each side a geodesic between consecutive corners and the last side returning to p1. FindInfraPolygon[graph, verts, count] returns exactly count or $Failed; UpTo[count] returns up to count; All returns all polygons (the Cartesian product of per-side geodesic realisations). Default count = 1 (first geodesic per side). Corners accept bare vertices or unary InfraPoint. Options Properties / Method forward to the side geodesics (see FindInfraSegment).";
FindInfraRegularPolygon::usage = "FindInfraRegularPolygon[graph, As, n] returns {InfraPolygon[{poly}]} for a length-n cyclic vertex sequence whose k-diagonal distances satisfy As[[k]] (slot grammar: Integer exact, {lo, hi} constant in range, Automatic any constant; Length[As] <= Floor[n/2]); count / UpTo[count] / All controls multiplicity. Options Properties ({}), Method (\"Exhaustive\"), \"From\" (All (default), v, v -> r).";
InfraPolygonQ::usage = "InfraPolygonQ[graph, poly] tests whether poly is a closed chain of geodesic InfraSegment sides (every leg a geodesic, consecutive legs sharing endpoints cyclically). Accepts an InfraPolygon wrapper or a bare leg list {seg1, ..., segn}.";
InfraRegularPolygonQ::usage = "InfraRegularPolygonQ[graph, cycle, As] tests whether the cycle is a regular n-gon w.r.t. the metric tuple As, with the same slot grammar as FindInfraRegularPolygon (Integer | {lo, hi} | Automatic per slot). Accepts open and closed input.";

(* ===================== InfraTriangle ===================== *)

InfraTriangle::usage = "InfraTriangle[{poly}] is the unary form, where poly = {seg1, seg2, seg3} is a closed chain of three unary InfraSegment sides; InfraTriangle[{poly1, ...}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations. Accessors [\"Sides\"], [\"Length\"], [\"Vertices\"] as for InfraPolygon.";
FindInfraTriangle::usage = "FindInfraTriangle[graph, {a, b, c}] returns {InfraTriangle[{poly}]} for the triangle through corners a, b, c (the n = 3 case of FindInfraPolygon); each side a geodesic. FindInfraTriangle[graph, verts, count] / UpTo[count] / All controls multiplicity over the Cartesian product of per-side geodesics; default count = 1. Options Properties / Method forward to the side geodesics (see FindInfraSegment).";
InfraTriangleQ::usage = "InfraTriangleQ[graph, poly] tests whether poly is a triangle (a closed chain of exactly three geodesic InfraSegment sides). Accepts an InfraTriangle wrapper or a bare three-leg list.";

(* ===================== InfraEllipticShell ===================== *)

InfraEllipticShell::usage = "InfraEllipticShell[{set}] is the unary form (one elliptic-shell vertex set); InfraEllipticShell[{set1, ..., setk}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations.";
FindInfraEllipticShell::usage = "FindInfraEllipticShell[graph, {p1, p2}, c] returns {InfraEllipticShell[{levelSet}]} for the elliptic shell { v : d(p1,v) + d(p2,v) == c } (c may be {cMin, cMax}). FindInfraEllipticShell[graph, {p1, p2}, c, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Options Properties (\"Separating\", \"Connected\"), Method (\"Exhaustive\" (default), \"Greedy\").";
InfraEllipticShellQ::usage = "InfraEllipticShellQ[graph, vertexSet] tests whether vertexSet is an elliptic shell, i.e. equals { v : d(p1,v) + d(p2,v) == c } for some p1, p2, c.";

(* ===================== InfraQuadric ===================== *)

FindInfraQuadric::usage = "FindInfraQuadric[graph, {p1, ..., pk}, c] returns InfraObject[{ v : sum_i d(p_i, v) <= c }], the solid ellipsoid interior with foci p_i. FindInfraQuadric[graph, foci, {cMin, cMax}] returns the band { v : cMin <= sum_i d(p_i, v) <= cMax }. FindInfraQuadric[graph, foci, c, weights] uses the weighted signed-distance sum sum_i w_i d(p_i, v); weights {1, -1} give a hyperboloid (one branch via scalar c, two-sided via symmetric band {-c, c}). Foci accept bare vertices or InfraPoint wrappers (multi-realisation reduced to first).";

(* ===================== InfraEllipse ===================== *)

InfraEllipse::usage = "InfraEllipse[{cycle}] is the unary form; InfraEllipse[{cycle1, ..., cyclek}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations.";
FindInfraEllipse::usage = "FindInfraEllipse[graph, {p1, p2}, c] returns {InfraEllipse[{cycle}]} for the shortest separating cycle in the level-surface subgraph { v : cMin <= d(p1,v) + d(p2,v) <= cMax } (c scalar or {cMin, cMax}). FindInfraEllipse[graph, {p1, p2}, c, n] returns exactly n realisations or $Failed; UpTo[n] returns up to n; All returns all. Option Properties (default {\"Separating\", \"Shortest\"}).";
InfraEllipseQ::usage = "InfraEllipseQ[graph, cycle] tests whether the vertex sequence cycle is a metric ellipse (cyclic edge chain whose vertex set is an elliptic shell). Accepts both open and closed input.";

(* ===================== InfraPlane ===================== *)

InfraPlane::usage = "InfraPlane[{set}] is the unary form (one bisecting hyperplane); InfraPlane[{set1, ..., setk}] is the multi-realisation form. Find* returns one wrapper carrying the requested realisations. Scene-language constructors InfraPlane[p1, p2] and InfraPlane[p1, p2, {lo, hi}] are used inside InfraScene.";
FindInfraBisectingHyperplane::usage = "FindInfraBisectingHyperplane[graph, p1, p2] returns {InfraPlane[{slab}]} for the bisector { v : d(p1, v) == d(p2, v) }; a positional {lo, hi} widens the slab to lo <= d(p1, v) - d(p2, v) <= hi. FindInfraBisectingHyperplane[graph, p1, p2, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Options Properties (\"Separating\", \"Connected\"), Method (\"Exhaustive\" (default), \"Greedy\").";

(* ===================== InfraRay ===================== *)

InfraRay::usage = "InfraRay[{ray}] is the unary form (one pointed half-line from a base vertex O); InfraRay[{ray1, ..., rayk}] is the multi-realisation form. Each ray is a vertex sequence {O, ..., w} with w an inextensible endpoint. Find* returns one wrapper carrying the requested realisations. Consumed by InfraSceneHighlight (sequential-edge semantics).";
FindInfraRay::usage = "FindInfraRay[graph, O, v] returns {InfraRay[{ray}]} for one pointed half-line from O in v's direction: the half of a maximal geodesic line through O and v starting at O and ending at the far inextensible endpoint. FindInfraRay[graph, O, v, n] returns a List of n unary InfraRay[{ray}] wrappers or $Failed; UpTo[n] / All controls multiplicity. O and v accept InfraPoint[{...}] for multi-anchor spread.";

(* ===================== InfraPolyline ===================== *)

InfraPolyline::usage = "InfraPolyline[{poly}] is the unary form: poly = {seg1, ..., segk} is a list of unary InfraSegment[{path_i}] with consecutive legs sharing their endpoint (Last[path_i] == First[path_{i+1}]). InfraPolyline[{poly1, ..., polym}] is the multi-realisation form. Consumed by InfraSceneHighlight (each realisation flattens to one concatenated vertex sequence; sequential-edge semantics).";
FindInfraPolylineSubdivision::usage = "FindInfraPolylineSubdivision[graph, path] returns {InfraPolyline[{{seg1, ..., segk}}]} where the legs are the fewest geodesic InfraSegments whose knots are path-vertices and each leg is a shortest path since the previous knot. Option: \"MaxLength\" (Infinity (default) | numeric L) caps every leg's graph-length at L.";
InfraPolylineQ::usage = "InfraPolylineQ[graph, poly] tests whether poly = {seg1, ..., segk} of unary InfraSegment wrappers is a valid polyline in graph: every leg is a geodesic and consecutive legs share an endpoint. Accepts the wrapped form InfraPolyline[{...}] as well (AllTrue over realisations).";

(* ===================== InfraRevolution ===================== *)

InfraObject::usage = "InfraObject[vs] wraps a vertex set vs as a single graph-geometric object; consumed by InfraSceneHighlight (induced-subgraph semantics).";
InfraRevolution::usage = "InfraRevolution[axis, profile] is the scene-language constructor for a rotational object inside InfraScene.";
FindInfraRevolution::usage = "FindInfraRevolution[graph, axis, profile] returns InfraObject[set] for the rotational vertex set around axis with the given radius profile (NumericQ constant, List, Association, or callable). Options \"Form\" (\"Solid\" (default), \"Surface\"), Method (\"Voronoi\" (default), \"PerpendicularBisector\", \"Balls\").";
FindInfraCylinder::usage = "FindInfraCylinder[graph, axis, r] returns InfraObject[set] for the constant-radius rotational set around axis. Defaults to Method -> \"Balls\" (the r-neighborhood of the axis, i.e. all v with d(v, c) <= r for some axis vertex c); pass Method -> \"Voronoi\" for the flat-capped variant. Inherits remaining FindInfraRevolution options.";
FindInfraCone::usage = "FindInfraCone[graph, axis, slope] returns InfraObject[set] for the cone of given slope with apex at axis[[1]]: radii are slope * Range[0, Length[axis] - 1]. Option \"Apex\" (First (default) | Last) flips the apex end; inherits remaining options from FindInfraRevolution.";
InfraRevolutionQ::usage = "InfraRevolutionQ[graph, vs, axis, profile] tests whether vs equals the rotational vertex set around axis with the given radius profile. Option \"Form\" matches FindInfraRevolution.";

(* ===================== EuclideanSpace ===================== *)

InfraScalarProduct::usage = "InfraScalarProduct[graph, o, u, v] returns the base-point-relative scalar product d(o,u) d(o,v) cos(theta_k). Option Method (\"Alexandrov\" (default; the k = 0 polar form), {\"Alexandrov\", \"Curvature\" -> k}, \"Parallelogram\").";
FindInfraLinearCombination::usage = "FindInfraLinearCombination[graph, o, {{lambda1, u1}, {lambda2, u2}, ...}] returns InfraPoint[{v1, ...}], the multi-valued vertex realisation of sum_i lambda_i u_i with base point o, computed as scaled-then-pairwise-summed left-to-right. The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed. Options: \"ScaleMethod\" (Automatic (default), \"Metric\", \"Line\", \"Midpoint\"); \"SumMethod\" (\"Metric\" (default), \"Parallel\").";
InfraAngle::usage = "InfraAngle[graph, {q1, p, q2}] returns the angle at p in radians. Option Method (\"Arclength\" (default; boundary arclength outside the ball, divided by the radius), \"Alexandrov\", {\"Alexandrov\", \"Curvature\" -> k}).";

(* ===================== InfraCurveGeometry ===================== *)

TurningAngles::usage = "TurningAngles[graph, path] returns the list of discrete exterior angles Pi - InfraAngle[graph, {v_{i-1}, v_i, v_{i+1}}] at each interior vertex of the polygonal curve path = {v_1, ..., v_k}; closed cycles (First[path] == Last[path]) include the wrap-around triple. TurningAngles[graph, polyline] returns the list of turning angles at the knot vertices of an InfraPolyline (one list per realisation); geodesic-interior vertices contribute no turning by construction.";
TotalCurvature::usage = "TotalCurvature[graph, path] returns the discrete total curvature Total @ TurningAngles[graph, path] of the polygonal curve.";
TotalAbsoluteCurvature::usage = "TotalAbsoluteCurvature[graph, path] returns Total @ Abs @ TurningAngles[graph, path], the discrete analogue of the Fenchel-side integral of |kappa|.";
TurningNumber::usage = "TurningNumber[graph, cycle] returns TotalCurvature[graph, cycle] / (2 Pi); integer in the smooth planar case, a real-valued empirical quantity on graphs.";

(* ===================== AlexandrovGeometry ===================== *)

ComparisonTriangle::usage = "ComparisonTriangle[a, b, c] returns the Wolfram Triangle in R^2 with side lengths {a, b, c} (a opposite p, b opposite q, c opposite r). ComparisonTriangle[a, b, c, \"Curvature\" -> k] for k != 0 returns InfraComparisonTriangle[<|\"Sides\" -> ..., \"Curvature\" -> k, \"Angles\" -> {alpha_p, alpha_q, alpha_r}|>] in M_k^2. ComparisonTriangle[graph, p, q, r, \"Curvature\" -> k] reads the side lengths from the graph.";
InfraComparisonTriangle::usage = "InfraComparisonTriangle[<|...|>] is the wrapper head for non-Euclidean comparison triangles returned by ComparisonTriangle. Accessors: [\"Sides\"], [\"Curvature\"], [\"Angles\"].";
CATInequalityQ::usage = "CATInequalityQ[graph, {p, q, r}, k : 0] tests whether the geodesic triangle on {p, q, r} satisfies the CAT(k) thinness inequality. Option Method (\"ApexSide\" (default) -- d(apex, x) <= d_k_bar(apex', x') for every interior vertex x on the side opposite to each apex; \"TwoRays\" -- d(x, y) <= d_k_bar(x', y') for every cross-ray pair (x, y) on the two rays emanating from each apex). Returns Indeterminate when k > 0 and the triangle perimeter exceeds 2 Pi / Sqrt[k].";
InfraCurvature::usage = "InfraCurvature[graph, v] returns the local Alexandrov upper-curvature bound at v: L^2 times the supremum of per-triangle CAT bounds over all triangles whose three vertices sit inside B_L(v), with L = GraphDiameter[graph] by default. Option: \"Radius\" (Automatic | Integer L) selects the ball radius; reported curvature is rescaled by L^2 to match the (edge / L)^-2 unit convention. InfraCurvature[graph] returns an Association[v -> kappa_v] over all vertices.";

(* ===================== PathSpace ===================== *)

SelectInfraPath::usage = "SelectInfraPath[graph, paths] draws one path from the bundle treated as a finite metric space in path-space. SelectInfraPath[graph, paths, n] returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole pool. Options \"From\" (All (default), \"Center\", \"Periphery\", \"MostVisited\", \"Bottleneck\", \"MinLength\", \"MaxLength\", anchor -> spec, {\"Min\" | \"Max\", scoreFn}), \"Distance\", \"Metric\" (\"Hausdorff\" (default), \"Frechet\", \"MeanFrechet\"), \"MaxCliques\". Operator form SelectInfraPath[graph, n, opts][paths].";
SelectInfraCycle::usage = "SelectInfraCycle[graph, cycles] draws one cycle from the bundle in path-space (rotation-invariant metrics); n / UpTo[n] / All controls multiplicity. Options as SelectInfraPath. Operator form SelectInfraCycle[graph, n, opts][cycles].";
EmbeddingClosest::usage = "EmbeddingClosest[graph, bundle, ref] keeps the bundle elements whose drawing under GraphEmbedding is Hausdorff-closest to the Euclidean reference: ref = {p1, p2} (segment), {center, radius} (circle), or a curve / point list. Wrappers are preserved; operator form EmbeddingClosest[graph, ref][bundle].";

FindEmbeddingClosestPath::usage = "FindEmbeddingClosestPath[graph, curve] snaps an arbitrary embedded curve to a graph walk and returns InfraPath[{walk}] tracing it: each sampled curve point is mapped to its nearest vertex under the graph embedding, consecutive repeats are dropped, and successive anchors are joined by geodesics. curve is a Line / BSplineCurve / BezierCurve or a list of plane points in the embedding's coordinates. Unlike EmbeddingClosest (which selects from a supplied bundle), this constructs the path, so it works for shapes with no enumerable bundle (spirals, figure-eights).";
GeodesicSprayGraph::usage = "GeodesicSprayGraph[graph, c] returns the BFS DAG rooted at c: a directed graph with edge u -> v whenever d(c, v) = d(c, u) + 1 and u-v is an edge of graph. Sinks are peripheral vertices reachable from c; directed paths c -> sink are exactly the maximal geodesics from c. GeodesicSprayGraph[graph, InfraPoint[{c1, ..., ck}]] uses multi-source BFS. GeodesicSprayGraph[graph, pairs] returns the union of geodesics between the listed vertex pairs. Options: \"AxisLength\" (truncate the source-spray DAG at depth k; default All), \"PathThickness\" (per-pair Hausdorff threshold; 0 keeps one geodesic per pair, Infinity keeps all), \"Directed\".";
PathSubgraph::usage = "PathSubgraph[graph, u, v] returns the union of all shortest u-v paths. PathSubgraph[graph, u, v, k] (or UpTo[k]) caps path length; PathSubgraph[graph, u, v, All] returns the full simple-path subgraph. Option: \"Directed\".";
FindForwardDeformation::usage = "FindForwardDeformation[graph, walk, spec] returns forward deformations of a walk A -> B (walks monotone in d(A, .)), grouped by the first-named axis of spec (\"LengthDelta\" / \"DeformationSize\"; each {x} exact, x upper bound, {lo, hi} range, bare name minimum); n / UpTo[n] / All sets the count per group.";

(* ===================== Homotopy ===================== *)

InfraHomotopy::usage = "InfraHomotopy[{chain}] is the unary form: one homotopy chain {p0, p1, ..., pk} of intermediate walks produced by k elementary moves from p0 to pk. InfraHomotopy[{chain1, ..., chaink}] is the multi-realisation form. Find* finders return one InfraHomotopy wrapper carrying the requested chains.";
FindInfraHomotopyRepresentative::usage = "FindInfraHomotopyRepresentative[graph, obj] returns {head[{w}]} for a length-shortest walk in obj's homotopy class (head matches the input wrapper; InfraCircle coerces to InfraString); n / UpTo[n] / All controls multiplicity. Options Method (\"Exhaustive\" (default), \"Greedy\"), \"FreeHomotopy\", \"NullHomotopicCycles\", \"MaxLength\", \"MaxMoves\".";
FindInfraHomotopyRepresentativeHomotopy::usage = "FindInfraHomotopyRepresentativeHomotopy[graph, obj] returns {InfraHomotopy[{chain}]} for a reduction chain of elementary moves ending at a representative; n / UpTo[n] / All controls multiplicity. Options as FindInfraHomotopyRepresentative.";
FindInfraHomotopy::usage = "FindInfraHomotopy[graph, a, b] returns {InfraHomotopy[{chain}]} of elementary moves from a to b (both must share a wrapper head; InfraCircle coerces to InfraString); n / UpTo[n] / All controls multiplicity. Options Method (\"Exhaustive\" (default), \"Greedy\"), \"FreeHomotopy\", \"NullHomotopicCycles\", \"MaxLength\", \"MaxMoves\".";
HomotopicQ::usage = "HomotopicQ[graph, a, b] tests whether a and b are in the same homotopy class. a and b must share a wrapper head (InfraPath / InfraLoop / InfraString; InfraCircle coerces to InfraString). Spreads over multi-realisation inputs as a Cartesian-AllTrue conjunction. Inherits FindInfraHomotopy options.";
NullHomotopicQ::usage = "NullHomotopicQ[graph, cycle] tests whether a closed walk is null-homotopic. Accepts a bare closed walk (auto-closed via {cycle, First[cycle]}), an InfraLoop[{...}], InfraString[{...}], or InfraCircle[{...}] (Cartesian-AllTrue over realisations). Inherits FindInfraHomotopy options.";
HomotopyMoveType::usage = "HomotopyMoveType[walk1, walk2] classifies the elementary move walk1 -> walk2 as \"Contract\" (walk shortens), \"Extend\" (walk lengthens), or \"Lateral\" (walks have the same length; only possible for moves that swap arcs of equal length on an even-length null-homotopic cycle).";
HomotopyMoveTypes::usage = "HomotopyMoveTypes[chain] applies HomotopyMoveType to each consecutive pair in chain. HomotopyMoveTypes[InfraHomotopy[{chain}]] returns the labels for the unary wrapper; HomotopyMoveTypes[InfraHomotopy[reps]] returns one label list per realisation.";

(* ===================== MetricAlgebra ===================== *)

MetricInterval::usage = "MetricInterval[graph, u, v] returns the vertex set { w : d(u, w) + d(w, v) == d(u, v) } -- the union of all geodesics from u to v.";
GeodesicMultiplicity::usage = "GeodesicMultiplicity[graph, u, v] returns the number of distinct geodesics from u to v, computed as (A^d)[u, v] where d = GraphDistance[graph, u, v].";
GeodesicMultiplicityMatrix::usage = "GeodesicMultiplicityMatrix[graph] returns {D, M} where D is the distance matrix and M[i, j] is the number of geodesics from vertex i to vertex j.";
MedianVertices::usage = "MedianVertices[graph, vs] returns the vertices minimising the sum of distances to vs. A graph is a median graph iff every triple has a unique median.";
FindSegmentHull::usage = "FindSegmentHull[graph, S] returns the smallest superset of S closed under MetricInterval (the segment operator), as an InfraSet. S is any Infra* object, a list of them, or a bare vertex list. Option \"LineStructure\" (None (default), or an InfraLineStructure / list of lines) closes under the chosen-geodesic stretch on that fixed family instead of all geodesics.";
SegmentHullQ::usage = "SegmentHullQ[graph, S] returns True if S is closed under the segment operator, i.e. equals its own FindSegmentHull. Option \"LineStructure\" as in FindSegmentHull.";

(* ===================== Visit measure ===================== *)

InfraMeasure::usage = "InfraMeasure[obj] returns the vertex occupation measure <|v -> appearances|> of an Infra* wrapper -- the marginal of its realization bundle onto its vertices. InfraMeasure[graph, obj] is graph-aware and unlocks the edge measure. Options \"On\" (\"Vertices\" (default), \"Edges\", \"Both\") and Method (\"Occupation\" (default) divides by numReps, the membership measure in [0,1]; \"Probability\" divides by total appearances, the distribution summing to 1).";

(* ===================== InfraSet ===================== *)

InfraSet::usage = "InfraSet[vs] wraps a vertex list vs as a set; coerces any Infra* wrapper to its underlying vertex set. Accessors: [\"Vertices\"] (the vertex list), [\"Length\"] (cardinality).";

FindInfraEquidistantSet::usage = "FindInfraEquidistantSet[graph, {p1, ..., pn}] returns the equidistant set {v : d(p1, v) == ... == d(pn, v)} as an InfraSet, the intersection of the n-1 consecutive perpendicular bisectors. FindInfraEquidistantSet[graph, {p1, ..., pn}, {lo, hi}] thickens each consecutive bisector to the slab lo <= d(p_i, v) - d(p_{i+1}, v) <= hi.";

InfraBoundary::usage = "InfraBoundary[g, s] is the boundary of the vertex set s (a bare vertex list or any Infra* object) in g, returned as an InfraSet. Option Method (\"Combinatorial\" (default), the inner vertex boundary via GraphBoundary; {\"Alexandrov\", \"Radius\" -> r}, the two-sided cl(s)\\int(s) in the closed-r-ball topology, default r = 1).";

InfraInterior::usage = "InfraInterior[g, s] is the interior of the vertex set s (a bare vertex list or any Infra* object) in g, returned as an InfraSet. Option Method (\"Combinatorial\" (default), s minus its inner boundary via GraphInterior; {\"Alexandrov\", \"Radius\" -> r}, the topological interior in the closed-r-ball topology, default r = 1).";

InfraVolume::usage = "InfraVolume[g, s] is the volume of s (a bare vertex list or any Infra* object). Options \"Volume\" (\"Hausdorff\" (default; |s| minus its boundary), \"Counting\", \"Boundary\"), Method (\"Combinatorial\" (default), {\"Alexandrov\", \"Radius\" -> r}).";

(* ===================== Coordinatization ===================== *)

(* RadarCoordinates / ResistanceCoordinates usage now lives in the Infrageometry paclet. *)

FindInfraRadarBasis::usage = "FindInfraRadarBasis is deprecated; use FindResolvingSet (Infrageometry paclet). FindInfraRadarBasis[graph, n, m] returns up to n resolving sets by ascending size.";
InfraRadarBasisQ::usage = "InfraRadarBasisQ is deprecated; use ResolvingSetQ (Infrageometry paclet). InfraRadarBasisQ[graph, basis] tests whether basis is a resolving set.";
OrthogonalCoordinates::usage = "OrthogonalCoordinates[graph, c, axes, v] returns the Z-valued displacement of v along each axis through the centre c; OrthogonalCoordinates[graph, c, axes] returns the Association over all vertices. Option \"SelectCoordinate\" (\"Centered\" (default) and other projection-tie reducers).";
FindInfraOrthogonalFrame::usage = "FindInfraOrthogonalFrame[graph, c, axisLength] returns one frame of mutually perpendicular InfraSegment axes through c (axisLength = All | n | UpTo[n] | {min, max}); a trailing n / UpTo[n] / All controls the frame count. Options Method, \"AxisCount\", \"BranchSampleSize\", \"SelectCoordinate\". For the no-centre form use FindInfraSpanningAxes.";
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

(* ===================== Scenes ===================== *)

InfraScene::usage = "InfraScene[objects, hypotheses] constructs a scene descriptor from symbolic objects and construction/assertion hypotheses. Properties: scene[\"Steps\"], [\"Constructions\"], [\"Assertions\"], [\"DependencyGraph\"].";
FindInfraScene::usage = "FindInfraScene[scene, graph] evaluates all construction steps and returns InfraInstance objects; the third argument can be n (cap step count) or an Association of pre-fixed bindings. Option: \"PruneProbability\".";
InfraInstance::usage = "InfraInstance[bindings] wraps a solved binding association from FindInfraScene. Read out via InfraInstance[bindings, sym] or InfraInstance[bindings, {sym1, sym2, ...}].";
InfraGeometricStep::usage = "InfraGeometricStep[{hyp1, hyp2, ...}] groups hypotheses into a manual construction step. InfraGeometricStep[{hyps...}, label] adds a label.";
InfraIntersection::usage = "InfraIntersection[obj1, obj2, ...] returns the vertex-set intersection of the given Infra* objects as an InfraSet; inside InfraScene hypotheses it stays inert until bindings resolve.";
InfraUnion::usage = "InfraUnion[obj1, obj2, ...] returns the vertex-set union of the given Infra* objects as an InfraSet.";
InfraDistance::usage = "InfraDistance[g, p, q] is the graph distance between p and q, each a bare vertex or any Infra* wrapper, aggregated over the underlying vertex sets via the \"Aggregation\" option (Min default). InfraDistance[p, q] is recognised inside InfraScene assertions.";
InfraPlaneQ::usage = "InfraPlaneQ[h, p1, p2] asserts inside InfraScene that h is a valid bisecting hyperplane between p1 and p2.";
InfraIntersectQ::usage = "InfraIntersectQ[s1, s2] asserts inside InfraScene that two sets intersect.";

(* ===================== Highlights / Viewers ===================== *)

$InfraPointColor::usage   = "Default highlight color for InfraPoint objects.";
$InfraSegmentColor::usage = "Default highlight color for InfraSegment objects.";
$InfraLineColor::usage    = "Default highlight color for InfraLine objects.";
$InfraShellColor::usage   = "Default highlight color for InfraShell objects.";
$InfraBallColor::usage    = "Default highlight color for InfraBall objects.";
$InfraPlaneColor::usage   = "Default highlight color for InfraPlane objects.";
$InfraCircleColor::usage  = "Default highlight color for InfraCircle objects.";
$InfraRayColor::usage     = "Default highlight color for InfraRay objects.";
$InfraPathColor::usage    = "Default highlight color for InfraPath, InfraLoop and InfraString objects.";
$InfraObjectColor::usage  = "Default highlight color for InfraObject objects.";
$InfraTopologyColor::usage = "Default highlight color for topology overlays.";
$InfraPalette::usage = "$InfraPalette is a Dataset of the default object colors, one row per primitive with the color, the symbol naming it, and the wrapper heads drawn in it. It is the source the $Infra*Color symbols and InfraSceneHighlight's head-to-color dispatch both read from.";

InfraSceneHighlight::usage = "InfraSceneHighlight[g, multiObjects] renders multi-objects diffusely on g: intensity scales with within-object multiplicity, colors blend across objects; InfraSceneHighlight[g, obj] is the single-object shortcut. Per-object style overrides via entry -> color | Directive | {opts}. Options \"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\" (each None | scalar base measure | {min, max} envelope).";
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] is an interactive visualisation of an InfraScene on a graph; an optional third Association of pre-fixed bindings is supported. Controls: step arrows with a label menu, a per-step hide toggle (eye), \"Branch\"/\"Diffuse\" mode setter, branch arrows, and a \"Fixed\" checkbox locking the current branch as the initial bindings of later steps. Rendering options (\"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\", ImageSize) forward to InfraSceneHighlight.";
PointViewer::usage = "PointViewer[g] is an interactive viewer for selecting points in graph g. PointViewer[g, sym] stores the current selection in sym.";
SegmentViewer::usage = "SegmentViewer[g] is an interactive viewer for exploring geodesic segments in graph g.";
ShellViewer::usage = "ShellViewer[g] is an interactive viewer for exploring metric shells in graph g. Method setter: \"Metric\" | \"Separating\".";
CircleViewer::usage = "CircleViewer[g] is an interactive viewer for exploring separating cycles in graph g.";

(* ===================== InfraEquality ===================== *)

InfraEqualQ::usage = "InfraEqualQ[graph, a, b] tests equality of two Infra* wrappers via their diffusion diagrams (vertex -> total-occurrence multisets). Returns False on head mismatch. Option Method (\"Diffuse\" (default; |A cap B| > |A delta B|, equivalently weighted Jaccard > 1/2) | \"Overlap\" (at least one common vertex) | \"Set\" (vertex sets identical) | \"Multiset\" (diffusion diagrams identical)).";
