Package["WolframInstitute`SyntheticInfrageometry`"]

(* Tools *)
HausdorffDistance::usage = "HausdorffDistance[d, setX, setY] computes the Hausdorff distance between two sets using a distance matrix or graph.";
FrechetDistance::usage = "FrechetDistance[d, setX, setY, f] computes distances between point sets using function f (default Max for Frechet distance).";
MinimalSeparationDistance::usage = "MinimalSeparationDistance[d, setX, setY] finds the minimum distance between two sets.";
EmbeddingHausdorffDistance::usage = "EmbeddingHausdorffDistance[coords, path, {p1, p2}] computes the Hausdorff distance between the piecewise-linear curve through coords[[path]] and the straight line segment from coords[[p1]] to coords[[p2]].";
EmbeddingCircleDistance::usage = "EmbeddingCircleDistance[coords, cycle, center, radius] computes Hausdorff distance between a cycle's embedding and a perfect circle.";
CentralElement::usage = "CentralElement[distanceMatrix, n] finds n most central elements in a distance matrix using maxmin criterion.";
PeripheralElement::usage = "PeripheralElement[distanceMatrix, n] finds n most peripheral elements using maxmin criterion.";
SegmentEndpoints::usage = "SegmentEndpoints[segment] returns the first and last elements of a segment.";
SeparatingCycleQ::usage = "SeparatingCycleQ[graph, cycle, center, radius] tests whether a cycle separates interior (distance <= radius from center) from exterior.";
FindSeparatingCycles::usage = "FindSeparatingCycles[graph, cycles, center, radius] selects cycles that separate interior from exterior around center.";

(* Postulates *)
FindPoint::usage = "FindPoint[graph] finds a random vertex. FindPoint[graph, n] finds up to n vertices. Options: \"From\" (\"Random\"|\"Center\"|\"Periphery\"), \"Distance\" (number, {min,max}, or \"Max\"), \"MaxCliques\".";
FindSegment::usage = "FindSegment[graph, p1, p2] finds all geodesics. FindSegment[graph, p1, p2, n] returns up to n. FindSegment[graph, {p1, p2}] also accepted. Option: \"Select\" sorts results by criterion.";
FindLine::usage = "FindLine[graph, p1, p2] finds all maximal geodesic extensions. FindLine[graph, p1, p2, n] returns up to n. Option: \"Select\" sorts results by criterion.";
FindCircle::usage = "FindCircle[graph, p, r] finds separating cycles at distance r from p. FindCircle[graph, p, {rMin, rMax}] uses a distance range. FindCircle[graph, p, r, n] returns up to n. Option: \"Select\" sorts results by criterion.";

(* Predicates *)
SegmentQ::usage = "SegmentQ[graph, segment] tests whether a list of vertices forms a valid geodesic segment.";
CircleQ::usage = "CircleQ[graph, cycle, center, radius] tests whether a cycle is a valid metric circle: connected, equidistant from center, and separating.";
LineQ::usage = "LineQ[graph, segment] tests whether a segment is maximal -- cannot be extended to a longer geodesic.";
IntersectQ::usage = "IntersectQ[set1, set2] tests whether two sets have a non-empty intersection.";
ParallelQ::usage = "ParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). ParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold.";
SegmentLineAngle::usage = "SegmentLineAngle[graph, p1, p2, line] or SegmentLineAngle[graph, segment, line] measures the distance from segment endpoint to a line.";

(* Coordinatization *)
FindMetricBasis::usage = "FindMetricBasis[graph, n, m] finds up to n metric bases. m specifies which basis sizes to try: All (default), an integer (max size), {min, max}, or {exact}.";
MetricBasisQ::usage = "MetricBasisQ[graph, basis] tests whether basis is a metric basis of graph.";
MetricCoordinates::usage = "MetricCoordinates[graph, vertex, basis] gives the distance coordinates of vertex with respect to basis.";
MetricBisector::usage = "MetricBisector[graph, {a, b}] returns the vertices equidistant from a and b.";

(* Scenes *)
InfraScene::usage = "InfraScene[objects, hypotheses] constructs a scene descriptor from symbolic objects and hypotheses (constructions and assertions). Access properties via scene[\"Steps\"], scene[\"Constructions\"], scene[\"Assertions\"], scene[\"DependencyGraph\"].";
FindInfraScene::usage = "FindInfraScene[scene, graph] evaluates all construction steps and returns a list of InfraInstance objects. FindInfraScene[scene, graph, n] evaluates up to n steps. Option: \"PruningProbability\" (default 0).";
InfraInstance::usage = "InfraInstance[bindings] wraps a solved binding association from FindInfraScene. Access bindings via instance[[1]][symbol].";
InfraPoint::usage = "InfraPoint[] represents any vertex. InfraPoint[v] fixes a vertex. InfraPoint[\"Center\"] or InfraPoint[\"Periphery\"] selects from graph center/periphery. InfraPoint[origin, dist] selects vertices at given distance. InfraPoint[n] finds n mutually distant vertices.";
InfraSegment::usage = "InfraSegment[p, q] represents geodesics between p and q. Used in InfraScene hypotheses.";
InfraLine::usage = "InfraLine[p, q] represents maximal geodesic extensions through p and q. InfraLine[path] extends a given path. Used in InfraScene hypotheses.";
InfraCircle::usage = "InfraCircle[center, radius] represents separating cycles at given radius from center. Used in InfraScene hypotheses.";
InfraIntersection::usage = "InfraIntersection[obj1, obj2] represents the vertex set intersection of two geometric objects. Used in InfraScene hypotheses.";
InfraIntersectionPoint::usage = "InfraIntersectionPoint[obj1, obj2] is an alias for InfraIntersection.";
InfraDistance::usage = "InfraDistance[p, q] represents graph distance between p and q. Used in InfraScene assertions.";
InfraSegmentQ::usage = "InfraSegmentQ[s] asserts that s is a valid geodesic segment. Used in InfraScene assertions.";
InfraCircleQ::usage = "InfraCircleQ[c, center, r] asserts that c is a valid metric circle. Used in InfraScene assertions.";
InfraLineQ::usage = "InfraLineQ[s] asserts that s is a maximal geodesic. Used in InfraScene assertions.";
InfraParallelQ::usage = "InfraParallelQ[l1, l2] asserts that two lines are parallel. Used in InfraScene assertions.";
InfraIntersectQ::usage = "InfraIntersectQ[s1, s2] asserts that two sets intersect. Used in InfraScene assertions.";

(* Interactive Viewers *)
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] creates an interactive visualization of an InfraScene on a graph. A step slider controls which construction steps are shown, a TogglerBar selects which objects to color, and vertex/edge coloring reflects branch frequency.";

(* Viewers *)
PointViewer::usage = "PointViewer[g] creates an interactive visualization of random selection of points in a graph given certain criteria. PointViewer[g, sym] stores the current selection in sym (updated dynamically).";
SegmentViewer::usage = "SegmentViewer[g] creates an interactive visualization for exploring geodesic segments between points in a graph.";
CircleViewer::usage = "CircleViewer[g] creates an interactive visualization for exploring circles (cycles) centered at a point in a graph.";
