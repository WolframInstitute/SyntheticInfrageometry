Package["WolframInstitute`SyntheticInfrageometry`"]

(* Usage messages are popup-sized: one definitional sentence per symbol, plus at
   most one short clause for a genuinely distinct call form. Option NAMES only
   (short value lists where the values are the description); no accessor essays,
   no method trees, no worked examples. Every Find* / Select* takes the same
   trailing count argument n | UpTo[n] | All -- stated only where the argument is
   easy to miss. Full detail lives in Wiki/Guides/ and the paclet documentation.
   See CLAUDE.md "Usage messages" for the rules. *)

(* ===================== InfraPoint ===================== *)

InfraEffectivePoint::usage = "InfraEffectivePoint[<|v -> m, ...|>] is a finitely supported measure on the vertex set -- the measure layer of the point ontology. Accessors \"Support\", \"Vertices\", \"Weights\", \"Mass\", \"Entropy\".";
InfraPoint::usage = "InfraPoint[v] is the point atom: one vertex of the substrate, carrying its label verbatim. Accessors \"Vertex\", \"Vertices\", \"Mass\", and graph-keyed invariants such as [\"BallVolumes\", g] and [\"Dimension\", g].";
FindInfraPoint::usage = "FindInfraPoint[graph] draws a point from the candidate pool; a trailing n | UpTo[n] | All sets the count. Options \"From\", \"Distance\", \"MaxCliques\".";
FindInfraMidpoint::usage = "FindInfraMidpoint[graph, p1, p2] gives the InfraEffectivePoint of the middle vertices of every geodesic from p1 to p2 (one vertex at even distance, two at odd). Option Method.";
FindInfraGoldenSection::usage = "FindInfraGoldenSection[graph, p1, p2] gives the InfraEffectivePoint at the golden-ratio index along every geodesic from p1 to p2. Option Method.";
FindInfraReflection::usage = "FindInfraReflection[graph, x, a] gives the reflections x' of x through a: the vertices with B(x, a, x') and d(a, x) == d(a, x').";
CompleteInfraEquilateralTriangle::usage = "CompleteInfraEquilateralTriangle[graph, p1, p2] gives the apexes equidistant from p1 and p2 at distance d(p1, p2) (Euclid I.1).";
FindInfraCommonPoint::usage = "FindInfraCommonPoint[graph, lines] gives the points lying on every listed line.";
FindClosestInfraPoint::usage = "FindClosestInfraPoint[graph, line, point] gives the points of line at minimum graph distance from point.";
SelectInfraPoint::usage = "SelectInfraPoint[graph, vertices] draws a point from a supplied bundle under graph distance; a trailing n | UpTo[n] | All sets the count. Options \"From\", \"Distance\", \"MaxCliques\".";
InfraReachableQ::usage = "InfraReachableQ[graph, p1, p2] tests whether p1 and p2 have realisations in the same connected component.";

(* ===================== InfraSegment ===================== *)

InfraSegment::usage = "InfraSegment[{path1, ...}] is a bundle of alternative geodesics; InfraSegment[dag] is the compact geodesic-DAG form. Accessors \"Graph\", \"Length\", \"Multiplicity\", \"Measure\", \"Realizations\".";
FindInfraSegment::usage = "FindInfraSegment[graph, p1, p2] gives one geodesic from p1 to p2; a trailing n | UpTo[n] enumerates that many, All the whole interval as an InfraSegment DAG. Option Method.";
ExtendInfraSegment::usage = "ExtendInfraSegment[graph, a, b, c, d] gives the points x with B(a, b, x) and d(b, x) == d(c, d) (Tarski axiom A4).";
InfraWalkQ::usage = "InfraWalkQ[graph, walk] tests whether walk is a walk: consecutive vertices adjacent (revisits allowed).";
InfraSegmentQ::usage = "InfraSegmentQ[graph, walk] tests whether walk is a geodesic.";
UniqueInfraSegmentQ::usage = "UniqueInfraSegmentQ[graph, u, v] tests whether the u-v geodesic is unique; UniqueInfraSegmentQ[graph] tests the geodetic property.";

(* ===================== InfraWalk ===================== *)

InfraWalk::usage = "InfraWalk[{walk1, ...}] is a bundle of walks, not necessarily simple. Inside InfraScene, InfraWalk[p1, ..., pk] is the step-wise constructor.";
FindInfraWalk::usage = "FindInfraWalk[graph, p1, kspec] grows the walks from p1 in the class cut by the Properties rules (default {\"Generic\"}, generic immersed) until a stopping condition or the budget kspec stops them; FindInfraWalk[graph, p1, p2, kspec] keeps those ending at p2. Options Properties, \"StoppingCondition\", Method.";
FindInfraGeodesic::usage = "FindInfraGeodesic[graph, p1, scale, kspec] grows the walks obeying a local rule in every window of scale consecutive vertices, as an InfraWalk; FindInfraGeodesic[graph, p1, p2, scale, kspec] keeps those ending at p2. Options Properties, \"StoppingCondition\", Method.";
InfraGeodesicQ::usage = "InfraGeodesicQ[graph, walk, scale] tests whether every window of scale consecutive vertices of walk plus the next one is a shortest path; scale 1 gives InfraWalkQ and Infinity gives InfraSegmentQ.";
WalkSingularities::usage = "WalkSingularities[walk] gives the singularity census of a walk as parameter data: \"Cusp\" apexes, \"Tangency\" range pairs of repeated arcs, \"Crossing\" isolated double visits, \"TriplePoint\" parameter lists, and \"EndpointIncidence\" (open) or \"Cover\" (closed heads).";
InfraImmersedQ::usage = "InfraImmersedQ[graph, walk] tests whether walk is an immersed walk: a walk with no cusp (no backtrack).";
InfraGenericQ::usage = "InfraGenericQ[graph, walk] tests whether walk is a generic immersed walk: no cusps, no tangencies, no triple points, endpoints off the curve (open) and singly covered (closed); isolated crossings allowed.";
ExtendInfraGeodesic::usage = "ExtendInfraGeodesic[graph, seed, scale, kspec] continues a seed walk under the geodesic rules at infra-scale scale until a stopping condition or the budget kspec (edges added per growing side) stops it. Options Properties, \"StoppingCondition\", Method, \"Direction\".";
ConcatenateInfraWalk::usage = "ConcatenateInfraWalk[path1, path2] joins every compatible walk pair, those with Last[walk1] === First[walk2].";
InfraLoop::usage = "InfraLoop[{walk1, ...}] is a bundle of closed walks with a fixed base point; open realisations are auto-closed.";
InfraString::usage = "InfraString[{walk1, ...}] is a bundle of closed walks modulo cyclic rotation -- the free-loop wrapper, stored in lex-least rotation.";

(* ===================== InfraLine ===================== *)

InfraLine::usage = "InfraLine[{line1, ...}] is a bundle of maximal geodesics; line[[i]] is the InfraPoint at position i across realisations. Inside InfraScene, InfraLine[p, q] is the constructor.";
FindInfraLine::usage = "FindInfraLine[graph, p1, p2] gives the maximal geodesics through p1 and p2; FindInfraLine[graph, segment] those containing segment. Options Method, \"Maximality\", \"Direction\".";
FindInfraParallel::usage = "FindInfraParallel[graph, line, p] gives the lines through p that stay at constant distance from line. Option Method.";
FindInfraPerpendicular::usage = "FindInfraPerpendicular[graph, line, point] gives the lines through point perpendicular to line. Options Method, \"Radius\".";
FindInfraCommonLine::usage = "FindInfraCommonLine[graph, vertices] gives the canonical lines containing every listed vertex.";
InfraLineQ::usage = "InfraLineQ[graph, walk] tests whether walk is a maximal geodesic.";
InfraParallelQ::usage = "InfraParallelQ[graph, l1, l2] tests whether two lines stay at constant distance; a trailing threshold allows that distance to vary.";
InfraPerpendicularQ::usage = "InfraPerpendicularQ[graph, l1, l2] tests whether two lines meet perpendicularly at every common vertex. Options Method, \"Radius\".";
PencilDirections::usage = "PencilDirections[graph, O] gives one canonical maximal geodesic per direction class at O.";
PencilCardinality::usage = "PencilCardinality[graph, O] gives the number of distinct direction classes at O.";
LineCount::usage = "LineCount[graph] gives the number of distinct canonical maximal geodesics in graph.";
FindLineHull::usage = "FindLineHull[graph, S] gives the smallest superset of S closed under the line operator. Option \"LineStructure\".";
LineHullQ::usage = "LineHullQ[graph, S] tests whether S is closed under the line operator.";
UniversalLineQ::usage = "UniversalLineQ[graph] tests whether some pair spans a line filling a whole connected component (Chen-Chvatal); UniversalLineQ[graph, {u, v}] tests one line.";

(* ===================== InfraLineStructure ===================== *)

InfraLineStructure::usage = "InfraLineStructure[{line1, ...}] is a consistent geodesic path system, stored as its maximal lines. Accessors \"Lines\", \"Paths\", \"Incidence\", \"Coordinates\", [\"Path\", u, v].";
FindLineStructure::usage = "FindLineStructure[graph] gives a consistent geodesic path system: one shortest path per vertex pair, with every stretch of a chosen path again chosen. Option Method sets the tie-breaking edge ranking.";
ConsistentPathSystemQ::usage = "ConsistentPathSystemQ[graph, obj] tests whether a geodesic path system is subpath-closed (Cizma-Linial consistent).";

(* ===================== InfraShell ===================== *)

InfraShell::usage = "InfraShell[{set1, ...}] is a bundle of metric shells -- level sets of the distance from a centre.";
FindInfraShell::usage = "FindInfraShell[graph, c, r] gives the metric shell { v : d(c, v) == r }; r may be a band {rmin, rmax}. Options Properties, Method.";
FindInfraOsculatingShell::usage = "FindInfraOsculatingShell[graph, path, i, k] gives the shells whose level set contains the k-vertex window of path centred at position i, one per osculating centre.";
FindAdvancingInfraFront::usage = "FindAdvancingInfraFront[graph, origin, steps] gives the foliation by a bouncing wavefront: each front steps one geodesic step outward and reflects inward where it cannot.";
FindInfraShellCenter::usage = "FindInfraShellCenter[graph, shell] recovers {center, radii} from a shell. Option Method.";
InfraShellQ::usage = "InfraShellQ[graph, vertexSet] tests whether vertexSet is a metric shell { v : d(c, v) == r } for some centre c and radius r.";
SeparatesQ::usage = "SeparatesQ[graph, vertexSet, u, v] tests whether deleting vertexSet disconnects u from v.";

(* ===================== InfraBall ===================== *)

InfraBall::usage = "InfraBall[{ball1, ...}] is a bundle of closed metric balls.";
FindInfraBall::usage = "FindInfraBall[graph, c, r] gives the closed ball { v : d(c, v) <= r }.";
InfraBallQ::usage = "InfraBallQ[graph, vertexSet] tests whether vertexSet is a closed metric ball.";
FindBallHull::usage = "FindBallHull[graph, S] gives the ball hull of S: the intersection of all closed balls containing S, the smallest ball-convex superset.";
BallHullQ::usage = "BallHullQ[graph, S] tests whether S is ball-convex, i.e. an intersection of closed balls.";

(* ===================== InfraCircle ===================== *)

InfraCircle::usage = "InfraCircle[{cycle1, ...}] is a bundle of metric circles, each an open vertex sequence with implicit wrap-around. InfraCircle[{dag1, ...}] is the pool form, one arc-folded geodesic DAG per atom. Accessors \"Realizations\", \"Length\", \"Multiplicity\", \"Measure\".";
FindInfraCircle::usage = "FindInfraCircle[graph, c, r] gives the shortest separating cycle in the level surface at radius r around c, together with its ties; with count All it gives the circle pool carrying them, falling back to the cycle sweep off the pool's certified class. Option Properties.";
FindInfraCycle::usage = "FindInfraCycle[graph, n] gives the n shortest simple cycles of graph; FindInfraCycle[graph, {kmin, kmax}, n] restricts their length.";
InfraCircleQ::usage = "InfraCircleQ[graph, cycle] tests whether cycle is a cyclic edge chain whose vertex set is a metric shell.";

(* ===================== InfraPolygon ===================== *)

InfraPolygon::usage = "InfraPolygon[{poly1, ...}] is a bundle of closed chains of geodesic InfraSegment sides. Accessors \"Sides\", \"Length\", \"Vertices\".";
FindInfraPolygon::usage = "FindInfraPolygon[graph, {p1, ..., pn}] gives the polygon with corners p1, ..., pn and a geodesic between each consecutive pair. Option Method.";
FindInfraRegularPolygon::usage = "FindInfraRegularPolygon[graph, As, n] gives the closed n-vertex sequences whose k-th diagonal distances all match As[[k]] (each slot an Integer, {lo, hi}, or Automatic). Options Method, \"From\".";
InfraPolygonQ::usage = "InfraPolygonQ[graph, poly] tests whether poly is a closed cyclic chain of geodesic sides.";
InfraRegularPolygonQ::usage = "InfraRegularPolygonQ[graph, cycle, As] tests whether cycle is regular with respect to the diagonal-distance tuple As.";

(* ===================== InfraTriangle ===================== *)

InfraTriangle::usage = "InfraTriangle[{poly1, ...}] is a bundle of geodesic triangles -- the n = 3 case of InfraPolygon. Accessors \"Sides\", \"Length\", \"Vertices\".";
FindInfraTriangle::usage = "FindInfraTriangle[graph, {a, b, c}] gives the triangle with corners a, b, c and a geodesic on each side. Option Method.";
InfraTriangleQ::usage = "InfraTriangleQ[graph, poly] tests whether poly is a closed chain of exactly three geodesic sides.";

(* ===================== InfraEllipticShell ===================== *)

InfraEllipticShell::usage = "InfraEllipticShell[{set1, ...}] is a bundle of elliptic shells -- level sets of a sum of distances to foci.";
FindInfraEllipticShell::usage = "FindInfraEllipticShell[graph, {p1, p2}, c] gives the elliptic shell { v : d(p1, v) + d(p2, v) == c }; c may be a band {cmin, cmax}. Options Properties, Method.";
InfraEllipticShellQ::usage = "InfraEllipticShellQ[graph, vertexSet] tests whether vertexSet is an elliptic shell for some pair of foci and some constant.";

(* ===================== InfraQuadric ===================== *)

FindInfraQuadric::usage = "FindInfraQuadric[graph, {p1, ..., pk}, c] gives the solid interior { v : Sum_i d(p_i, v) <= c }; a trailing weight list gives the signed sum, so weights {1, -1} give a hyperboloid branch.";

(* ===================== InfraEllipse ===================== *)

InfraEllipse::usage = "InfraEllipse[{cycle1, ...}] is a bundle of metric ellipses -- cycles lying on an elliptic shell.";
FindInfraEllipse::usage = "FindInfraEllipse[graph, {p1, p2}, c] gives the shortest separating cycle in the level surface { v : d(p1, v) + d(p2, v) == c }. Option Properties.";
InfraEllipseQ::usage = "InfraEllipseQ[graph, cycle] tests whether cycle is a cyclic edge chain whose vertex set is an elliptic shell.";

(* ===================== InfraPlane ===================== *)

InfraPlane::usage = "InfraPlane[{set1, ...}] is a bundle of bisecting hyperplanes.";
FindInfraBisectingHyperplane::usage = "FindInfraBisectingHyperplane[graph, p1, p2] gives the perpendicular bisector { v : d(p1, v) == d(p2, v) }; a positional {lo, hi} widens it to a slab. Options Properties, Method.";

(* ===================== InfraRay ===================== *)

InfraRay::usage = "InfraRay[{ray1, ...}] is a bundle of pointed half-lines, each running from a base vertex to an inextensible endpoint.";
FindInfraRay::usage = "FindInfraRay[graph, O, v] gives the half-line from O through v: the pointed half of a maximal geodesic through O and v.";
InfraRayQ::usage = "InfraRayQ[graph, ray] tests whether ray is a pointed half-line: a geodesic from its own first vertex that cannot be prolonged past its last.";

(* ===================== InfraPolyline ===================== *)

InfraPolyline::usage = "InfraPolyline[{poly1, ...}] is a bundle of chains of geodesic InfraSegment legs meeting at shared endpoints. Accessors \"Length\", \"Knots\".";
FindInfraPolylineSubdivision::usage = "FindInfraPolylineSubdivision[graph, path] chunks a walk into the fewest geodesic legs whose knots are walk vertices. Option \"MaxLength\" caps each leg.";
InfraPolylineQ::usage = "InfraPolylineQ[graph, poly] tests whether every leg is a geodesic and consecutive legs share an endpoint.";

(* ===================== InfraRevolution ===================== *)

InfraObject::usage = "InfraObject[vs] wraps a bare vertex set as a single graph-geometric object.";
InfraRevolution::usage = "InfraRevolution[axis, profile] is the InfraScene constructor for a solid of revolution.";
FindInfraRevolution::usage = "FindInfraRevolution[graph, axis, profile] gives the rotational vertex set around axis with the given radius profile, a constant, list, association, or function. Options \"Form\", Method.";
FindInfraCylinder::usage = "FindInfraCylinder[graph, axis, r] gives the constant-radius solid of revolution around axis, by default the r-neighbourhood of the axis.";
FindInfraCone::usage = "FindInfraCone[graph, axis, slope] gives the cone of the given slope with apex at one end of axis. Option \"Apex\".";
InfraRevolutionQ::usage = "InfraRevolutionQ[graph, vs, axis, profile] tests whether vs is the solid of revolution around axis with the given profile. Option \"Form\".";

(* ===================== EuclideanSpace ===================== *)

InfraScalarProduct::usage = "InfraScalarProduct[graph, o, u, v] gives the base-point-relative product d(o, u) d(o, v) cos(theta), at curvature 0 the polar form (d(o,u)^2 + d(o,v)^2 - d(u,v)^2)/2. Option Method.";
FindInfraLinearCombination::usage = "FindInfraLinearCombination[graph, o, {{lambda1, u1}, ...}] gives the vertex realisations of Sum_i lambda_i u_i based at o. Options \"ScaleMethod\", \"SumMethod\".";
InfraAngle::usage = "InfraAngle[graph, {q1, p, q2}] gives the angle at p in radians. Option Method (\"Arclength\", \"Alexandrov\").";

(* ===================== InfraCurveGeometry ===================== *)

TurningAngles::usage = "TurningAngles[graph, path] gives the exterior angles Pi - InfraAngle at each interior vertex of path; for an InfraPolyline, the angles at its knots.";
TotalCurvature::usage = "TotalCurvature[graph, path] gives Total @ TurningAngles[graph, path], the discrete total curvature of path.";
TotalAbsoluteCurvature::usage = "TotalAbsoluteCurvature[graph, path] gives Total @ Abs @ TurningAngles[graph, path], the discrete Fenchel integral of |kappa|.";
TurningNumber::usage = "TurningNumber[graph, cycle] gives TotalCurvature[graph, cycle] / (2 Pi).";

(* ===================== AlexandrovGeometry ===================== *)

ComparisonTriangle::usage = "ComparisonTriangle[a, b, c] gives the Euclidean Triangle with side lengths a, b, c; ComparisonTriangle[graph, p, q, r] reads the sides from the graph. Option \"Curvature\" places it in M_k^2.";
InfraComparisonTriangle::usage = "InfraComparisonTriangle[<|...|>] is the wrapper for comparison triangles of nonzero curvature. Accessors \"Sides\", \"Curvature\", \"Angles\".";
CATInequalityQ::usage = "CATInequalityQ[graph, {p, q, r}, k] tests whether the geodesic triangle on p, q, r satisfies the CAT(k) thinness inequality. Option Method (\"ApexSide\", \"TwoRays\").";
InfraCurvature::usage = "InfraCurvature[graph, v] gives the local Alexandrov upper curvature bound at v: the supremum of per-triangle CAT bounds inside a ball around v. Option \"Radius\".";

(* ===================== WalkSpace ===================== *)

SelectInfraWalk::usage = "SelectInfraWalk[graph, walks] draws a walk (or cycle, for a closed-head bundle) from a bundle treated as a metric space. Options \"From\", \"Distance\", \"Metric\", \"MaxCliques\", \"Cyclic\".";
EmbeddingClosest::usage = "EmbeddingClosest[graph, bundle, ref] keeps the bundle elements drawn closest to a Euclidean reference under GraphEmbedding; ref is {p1, p2}, {center, radius}, or a curve.";
FindEmbeddingClosestPath::usage = "FindEmbeddingClosestPath[graph, curve] snaps an embedded curve to a walk, mapping sampled points to nearest vertices and joining them by geodesics.";
GeodesicSprayGraph::usage = "GeodesicSprayGraph[graph, c] gives the BFS DAG rooted at c, whose directed source-to-sink paths are exactly the maximal geodesics from c; GeodesicSprayGraph[graph, pairs] gives the union of geodesics between listed pairs.";
PathSubgraph::usage = "PathSubgraph[graph, u, v] gives the union of all shortest u-v paths; a trailing length cap or All widens it to longer simple paths.";
FindInfraMonotoneDeformation::usage = "FindInfraMonotoneDeformation[graph, ref, deltaSpec] gives the walks between ref's endpoints along which a potential never decreases; deltaSpec bounds the edge count relative to ref. Option \"Potential\".";
InfraDeformationSize::usage = "InfraDeformationSize[ref, walk] gives the number of ref edges that walk replaces -- Length[ref] - 1 less the shared prefix and suffix.";

(* ===================== Homotopy ===================== *)

InfraHomotopy::usage = "InfraHomotopy[{chain1, ...}] is a bundle of homotopy chains, each a sequence of walks related by elementary moves.";
FindInfraHomotopyRepresentative::usage = "FindInfraHomotopyRepresentative[graph, obj] gives a length-shortest walk in obj's homotopy class. Options Method, \"FreeHomotopy\", \"NullHomotopicCycles\", \"MaxLength\", \"MaxMoves\".";
FindInfraHomotopyRepresentativeHomotopy::usage = "FindInfraHomotopyRepresentativeHomotopy[graph, obj] gives the chain of elementary moves reducing obj to a shortest representative. Options as FindInfraHomotopyRepresentative.";
FindInfraHomotopy::usage = "FindInfraHomotopy[graph, a, b] gives a chain of elementary moves from a to b; both must share a wrapper head. Options as FindInfraHomotopyRepresentative.";
HomotopicQ::usage = "HomotopicQ[graph, a, b] tests whether a and b lie in the same homotopy class.";
NullHomotopicQ::usage = "NullHomotopicQ[graph, cycle] tests whether a closed walk is null-homotopic.";
HomotopyMoveType::usage = "HomotopyMoveType[walk1, walk2] classifies an elementary move as \"Contract\", \"Extend\", or \"Lateral\".";
HomotopyMoveTypes::usage = "HomotopyMoveTypes[chain] applies HomotopyMoveType to each consecutive pair of a homotopy chain.";

(* ===================== MetricAlgebra ===================== *)

MetricInterval::usage = "MetricInterval[graph, u, v] gives { w : d(u, w) + d(w, v) == d(u, v) }, the union of all geodesics from u to v.";
GeodesicMultiplicity::usage = "GeodesicMultiplicity[graph, u, v] gives the number of distinct geodesics from u to v.";
GeodesicMultiplicityMatrix::usage = "GeodesicMultiplicityMatrix[graph] gives {D, M} with D the distance matrix and M the matrix of geodesic counts.";
MedianVertices::usage = "MedianVertices[graph, vs] gives the vertices minimising the sum of distances to vs.";
FindSegmentHull::usage = "FindSegmentHull[graph, S] gives the smallest superset of S closed under MetricInterval -- the geodesic convex hull. Option \"LineStructure\".";
SegmentHullQ::usage = "SegmentHullQ[graph, S] tests whether S is geodesically convex.";

(* ===================== Visit measure ===================== *)

InfraMeasure::usage = "InfraMeasure[obj] gives the occupation measure <|v -> appearances|> of an Infra* bundle; InfraMeasure[graph, obj] also unlocks the edge measure. Options \"On\", Method.";

(* ===================== InfraSet ===================== *)

InfraSet::usage = "InfraSet[vs] wraps a vertex list as a set, coercing any Infra* wrapper to its underlying vertex set. Accessors \"Vertices\", \"Length\".";
FindInfraEquidistantSet::usage = "FindInfraEquidistantSet[graph, {p1, ..., pn}] gives { v : d(p1, v) == ... == d(pn, v) }; a trailing {lo, hi} thickens each bisector to a slab.";
InfraBoundary::usage = "InfraBoundary[graph, s] gives the boundary of a vertex set or Infra* object. Option Method (\"Combinatorial\", \"Alexandrov\").";
InfraInterior::usage = "InfraInterior[graph, s] gives the interior of a vertex set or Infra* object. Option Method (\"Combinatorial\", \"Alexandrov\").";
InfraVolume::usage = "InfraVolume[graph, s] gives the volume of a vertex set or Infra* object. Options \"Volume\" (\"Hausdorff\", \"Counting\", \"Boundary\"), Method.";

(* ===================== Coordinatization ===================== *)

FindInfraRadarBasis::usage = "FindInfraRadarBasis[graph, n, m] gives resolving sets by ascending size. Deprecated: use FindResolvingSet in the Infrageometry paclet.";
InfraRadarBasisQ::usage = "InfraRadarBasisQ[graph, basis] tests whether basis is a resolving set. Deprecated: use ResolvingSetQ in the Infrageometry paclet.";
OrthogonalCoordinates::usage = "OrthogonalCoordinates[graph, c, axes, v] gives the integer displacement of v along each axis through the centre c; without v, the association over all vertices. Option \"SelectCoordinate\".";
FindInfraOrthogonalFrame::usage = "FindInfraOrthogonalFrame[graph, c, axisLength] gives frames of mutually perpendicular geodesic axes through the centre c. Options Method, \"AxisCount\", \"BranchSampleSize\", \"SelectCoordinate\".";
FindInfraSpanningAxes::usage = "FindInfraSpanningAxes[graph, n] gives n mutually well-separated longest geodesics across graph, with no fixed centre. Options \"AxisDistance\", \"MinLength\", \"MinSeparation\", \"AxisThickness\", \"RandomPick\".";

(* ===================== TarskiGeometry ===================== *)

BetweennessQ::usage = "BetweennessQ[graph, u, w, v] tests Tarski betweenness B(u, w, v): w lies on a geodesic from u to v.";
EquidistanceQ::usage = "EquidistanceQ[graph, a, b, c, d] tests Tarski equidistance d(a, b) == d(c, d).";
TarskiStructure::usage = "TarskiStructure[graph] gives a memoized association of the Tarski primitives: vertices, distances, betweenness, equidistance, diameter.";
TarskiBetweennessTensor::usage = "TarskiBetweennessTensor[graph] gives the sparse rank-3 tensor whose nonzero entries are the triples with B(v_i, v_j, v_k).";
TarskiEquidistanceClasses::usage = "TarskiEquidistanceClasses[graph] gives the partition of unordered vertex pairs by distance value.";
TarskiCongruenceReflexivityQ::usage = "TarskiCongruenceReflexivityQ[graph] tests Tarski axiom A1, ab == ba. Always True on undirected simple graphs.";
TarskiCongruenceTransitivityQ::usage = "TarskiCongruenceTransitivityQ[graph] tests Tarski axiom A2, transitivity of congruence. A tautology of equality.";
TarskiCongruenceIdentityQ::usage = "TarskiCongruenceIdentityQ[graph] tests Tarski axiom A3, ab == cc implies a == b. Holds on connected simple graphs.";
TarskiSegmentConstructionQ::usage = "TarskiSegmentConstructionQ[graph] tests Tarski axiom A4, segment construction. Generally False on finite graphs.";
TarskiFiveSegmentsQ::usage = "TarskiFiveSegmentsQ[graph] tests Tarski axiom A5, five segments. Holds on median graphs. Option \"MaxTuples\" caps the O(n^8) search.";
TarskiBetweennessIdentityQ::usage = "TarskiBetweennessIdentityQ[graph] tests Tarski axiom A6, B(a, b, a) implies a == b. Always True on connected simple graphs.";
TarskiInnerPaschQ::usage = "TarskiInnerPaschQ[graph] tests Tarski axiom A7, inner Pasch. Holds on median graphs; fails on cycles of length >= 5 and on Petersen.";
TarskiLowerDimensionQ::usage = "TarskiLowerDimensionQ[graph] tests Tarski axiom A8, the existence of three non-collinear points.";
TarskiUpperDimensionQ::usage = "TarskiUpperDimensionQ[graph] tests Tarski axiom A9, three points equidistant from two distinct points are collinear. False in effective dimension >= 3.";
TarskiEuclidAxiomQ::usage = "TarskiEuclidAxiomQ[graph] tests Tarski axiom A10, the parallel-axiom variant. Stub: returns Indeterminate.";
TarskiContinuityQ::usage = "TarskiContinuityQ[graph] tests Tarski axiom A11, Dedekind continuity. Always False on finite graphs.";
TarskiAxiomQ::usage = "TarskiAxiomQ[graph] gives the per-axiom results of all eleven Tarski axiom predicates.";
FindTarskiCounterexample::usage = "FindTarskiCounterexample[graph, predQ] gives vertex tuples witnessing the failure of a Tarski axiom predicate.";

(* ===================== ProjectiveGeometry ===================== *)

SameDirectionQ::usage = "SameDirectionQ[graph, O, v, w] tests whether v and w lie in the same direction at O, i.e. on a common maximal geodesic through O.";
CollinearQ::usage = "CollinearQ[graph, vertices] tests whether all listed vertices lie on a common line.";
ConcurrentQ::usage = "ConcurrentQ[graph, lines] tests whether all listed lines share a common vertex.";
UniquePencilQ::usage = "UniquePencilQ[graph, O] tests whether every direction at O is single-valued.";
UniqueCollinearQ::usage = "UniqueCollinearQ[graph, vertices] tests whether the listed vertices lie on a unique common line.";
UniqueConcurrentQ::usage = "UniqueConcurrentQ[graph, lines] tests whether the listed lines share exactly one common vertex.";
WhiteheadW1Q::usage = "WhiteheadW1Q[graph] tests Whitehead axiom W1: every line has at least three vertices.";
WhiteheadW2Q::usage = "WhiteheadW2Q[graph] tests Whitehead axiom W2: any two distinct vertices lie on exactly one line.";
WhiteheadW3Q::usage = "WhiteheadW3Q[graph] tests Whitehead axiom W3, the intersection property. O(|V|^4); use on small graphs.";
ProjectivePlaneGraphQ::usage = "ProjectivePlaneGraphQ[graph] tests whether graph is a synthetic projective plane: W1, W2, W3 and non-degeneracy.";

(* ===================== Enumeration ===================== *)

EnumerateGraphs::usage = "EnumerateGraphs[n, predQ] gives the connected n-vertex graphs from GraphData satisfying predQ. Option \"From\" supplies a different generator.";

(* ===================== Scenes ===================== *)

InfraScene::usage = "InfraScene[objects, hypotheses] builds a scene descriptor from symbolic objects and construction or assertion hypotheses. Properties \"Steps\", \"Constructions\", \"Assertions\", \"DependencyGraph\".";
FindInfraScene::usage = "FindInfraScene[scene, graph] solves a scene on a graph and gives the resulting InfraInstance bindings. Option \"PruneProbability\".";
InfraInstance::usage = "InfraInstance[bindings] wraps a solved binding association; InfraInstance[bindings, sym] reads one object out of it.";
InfraGeometricStep::usage = "InfraGeometricStep[{hyp1, ...}] groups hypotheses into one construction step of a scene; a second argument labels it.";
InfraIntersection::usage = "InfraIntersection[obj1, obj2, ...] gives the vertex-set intersection of Infra* objects as an InfraSet.";
InfraUnion::usage = "InfraUnion[obj1, obj2, ...] gives the vertex-set union of Infra* objects as an InfraSet.";
InfraDistance::usage = "InfraDistance[graph, p, q] gives the graph distance between two Infra* objects, aggregated over their vertex sets. Option \"Aggregation\".";
InfraPlaneQ::usage = "InfraPlaneQ[graph, h, p1, p2] tests whether h lies in the bisector slab of p1, p2 and separates them; a trailing window widens the slab. The graph-free InfraPlaneQ[h, p1, p2] is the inert InfraScene assertion.";
InfraIntersectQ::usage = "InfraIntersectQ[s1, s2] asserts inside an InfraScene that two sets intersect; it stays inert until bindings resolve, which is why it exists rather than the built-in IntersectingQ.";

(* ===================== Highlights / Viewers ===================== *)

$InfraPointColor::usage   = "Default highlight color for InfraPoint objects.";
$InfraSegmentColor::usage = "Default highlight color for InfraSegment objects.";
$InfraLineColor::usage    = "Default highlight color for InfraLine objects.";
$InfraShellColor::usage   = "Default highlight color for InfraShell objects.";
$InfraBallColor::usage    = "Default highlight color for InfraBall objects.";
$InfraPlaneColor::usage   = "Default highlight color for InfraPlane objects.";
$InfraCircleColor::usage  = "Default highlight color for InfraCircle objects.";
$InfraRayColor::usage     = "Default highlight color for InfraRay objects.";
$InfraWalkColor::usage    = "Default highlight color for InfraWalk, InfraLoop and InfraString objects.";
$InfraObjectColor::usage  = "Default highlight color for InfraObject objects.";
$InfraTopologyColor::usage = "Default highlight color for topology overlays.";
$InfraPalette::usage = "$InfraPalette is the Dataset of default object colors, one row per primitive; the source both the $Infra*Color symbols and InfraSceneHighlight read from.";

InfraSceneHighlight::usage = "InfraSceneHighlight[graph, objects] renders Infra* objects diffusely on graph, intensity scaling with multiplicity and colors blending across objects. Options \"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\", \"Arrowheads\" (Automatic = off; True = Arrowheads[Medium]; or an explicit head spec -- one head at the end of each path object, sized from the plot rather than the stroke).";
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] is an interactive step-by-step visualisation of an InfraScene on a graph.";
PointViewer::usage = "PointViewer[graph] is an interactive viewer for selecting points; PointViewer[graph, sym] stores the selection in sym.";
SegmentViewer::usage = "SegmentViewer[graph] is an interactive viewer for exploring geodesic segments.";
ShellViewer::usage = "ShellViewer[graph] is an interactive viewer for exploring metric shells.";
CircleViewer::usage = "CircleViewer[graph] is an interactive viewer for exploring separating cycles.";

(* ===================== InfraEquality ===================== *)

InfraEqualQ::usage = "InfraEqualQ[graph, a, b] tests equality of two Infra* objects through their diffusion diagrams. Option Method (\"Diffuse\", \"Overlap\", \"Set\", \"Multiset\").";

$InfraPointSizes::usage = "$InfraPointSizes is the association Small -> 4, Medium -> 7, Large -> 10 of absolute vertex-dot sizes. One value per class, independent of the graph.";
$InfraAccentPointSize::usage = "$InfraAccentPointSize is the absolute dot size (12) of the accent / centre role, which is not a size class and combines with Haloing[].";
