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
