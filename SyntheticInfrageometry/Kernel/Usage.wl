Package["WolframInstitute`SyntheticInfrageometry`"]

(* EuclideanPostulates *)
FindPoint::usage = "FindPoint[graph, n] finds exactly n vertices or returns $Failed. FindPoint[graph, UpTo[n]] returns up to n. FindPoint[graph] returns one vertex. Options: \"From\" (\"Random\"|\"Center\"|\"Periphery\"), \"Distance\" (number, {min,max}, or \"Max\"), \"MaxCliques\".";
FindSegment::usage = "FindSegment[graph, p1, p2] returns one geodesic or $Failed. FindSegment[graph, p1, p2, n] returns exactly n or $Failed; FindSegment[graph, p1, p2, UpTo[n]] returns up to n; FindSegment[graph, p1, p2, All] returns all geodesics. Option: \"Select\" sorts results by criterion.";
FindLine::usage = "FindLine[graph, p1, p2] returns one maximal geodesic extension through p1 and p2 or $Failed. FindLine[graph, p1, p2, n] returns exactly n or $Failed; FindLine[graph, p1, p2, UpTo[n]] returns up to n; FindLine[graph, p1, p2, All] returns all. Option: \"Select\" sorts results by criterion.";
FindSphere::usage = "FindSphere[graph, c, r] returns one separating cycle at distance r from c or $Failed. FindSphere[graph, c, r, n] returns exactly n or $Failed; FindSphere[graph, c, r, UpTo[n]] returns up to n; FindSphere[graph, c, r, All] returns all. Options: Method (\"MetricCircle\" returns the equidistant level set; \"SeparatingCycle\" (default) returns 2D-style separating cycles); \"Select\" sorts results.";

(* EuclideanPredicates *)
SegmentQ::usage = "SegmentQ[graph, segment] tests whether a list of vertices forms a valid geodesic segment.";
LineQ::usage = "LineQ[graph, segment] tests whether a segment is maximal -- cannot be extended to a longer geodesic.";
SphereQ::usage = "SphereQ[graph, vertexSet] tests whether vertexSet is a valid metric sphere -- equidistant from some center and separating its interior from its exterior. Center and radius are derived; use FindSphereParameters to obtain them.";
ParallelQ::usage = "ParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). ParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold.";
FindSphereParameters::usage = "FindSphereParameters[graph, vertexSet] returns the list of {center, radius} pairs consistent with vertexSet being a metric sphere.";

(* Coordinatization *)
FindRadarBasis::usage = "FindRadarBasis[graph, n, m] finds up to n spatial radar bases (resolving sets). m specifies which basis sizes to try: All (default), an integer (max size), {min, max}, or {exact}.";
RadarBasisQ::usage = "RadarBasisQ[graph, basis] tests whether basis is a spatial radar basis of graph (every vertex has a unique distance vector to the basis).";
RadarCoordinates::usage = "RadarCoordinates[graph, vertex, basis] returns the spatial radar coordinates of vertex: the vector of graph distances from vertex to each basis point.";
AxesCoordinates::usage = "AxesCoordinates[graph, axes, v] returns the tuple of layer indices, one per axis, of v's projection onto each axis (non-negative integers from each axis's path start). Each axis is either a vertex list (line) or a Graph (DAG). AxesCoordinates[graph, axes] returns an Association of coordinates for all graph vertices. AxesCoordinates[graph, c, v] auto-discovers orthogonal axes through c and returns the signed displacement (Z-valued) of v from c along each axis. AxesCoordinates[graph, c] returns an Association of signed coordinates for all vertices, with c at the origin. Options: Method -> \"ShortestPaths\" (default; orthogonal projection by minimum graph distance) or \"ParallelLines\" (placeholder); \"Origin\" -> vertex (signs coordinates relative to that origin when explicit axes are given).";
FindOrthogonalAxes::usage = "FindOrthogonalAxes[graph, n] returns n mutually well-separated longest paths in graph (or $Failed if fewer exist); FindOrthogonalAxes[graph, UpTo[n]] returns up to n; FindOrthogonalAxes[graph, All] returns all. FindOrthogonalAxes[graph, v, n] (or UpTo / All) constrains every axis to pass through vertex v as an interior point, anchoring a sign convention for downstream Z-valued coordinates. Options: \"DistanceFunction\" (\"MinEndpoint\" | \"Hausdorff\" | \"Separation\"), \"MinLength\" (Automatic = diameter), \"MinSeparation\" (Automatic = half the axis length), \"AxisThickness\" (axes within this Hausdorff distance of a chosen one are kept together), \"RandomPick\" (False picks the first match deterministically; True picks at random).";

(* ProjectivePostulates *)
FindPencil::usage = "FindPencil[graph, O] returns the pencil at vertex O: an Association whose keys are canonical maximal-geodesic lines through O (one per direction class) and whose values are the same vertex sets. A vertex distinct from O may appear under multiple keys when it lies on more than one maximal geodesic through O.";
PencilDirections::usage = "PencilDirections[graph, O] returns the list of canonical lines through O, one per direction class (the keys of FindPencil).";
PencilCardinality::usage = "PencilCardinality[graph, O] returns the number of distinct directions in the pencil at O.";
LineCount::usage = "LineCount[graph] returns the number of distinct canonical maximal geodesics (lines) in the graph.";
FindCommonLine::usage = "FindCommonLine[graph, vertices] returns one canonical line containing every listed vertex or $Failed. FindCommonLine[graph, vertices, n] returns exactly n or $Failed; FindCommonLine[graph, vertices, UpTo[n]] returns up to n; FindCommonLine[graph, vertices, All] returns all such lines.";
FindCommonPoint::usage = "FindCommonPoint[graph, lines] returns one vertex common to every listed line or $Failed. FindCommonPoint[graph, lines, n] returns exactly n or $Failed; FindCommonPoint[graph, lines, UpTo[n]] returns up to n; FindCommonPoint[graph, lines, All] returns all common vertices.";

(* ProjectivePredicates *)
SameDirectionQ::usage = "SameDirectionQ[graph, O, v, w] tests whether v and w lie in the same direction at O -- some maximal geodesic through O contains both.";
CollinearQ::usage = "CollinearQ[graph, vertices] tests whether all listed vertices lie on a common line.";
ConcurrentQ::usage = "ConcurrentQ[graph, lines] tests whether all listed lines share a common vertex.";
UniquePencilQ::usage = "UniquePencilQ[graph, O] tests whether every direction at O is single-valued (FindLine[graph, O, v] returns exactly one line for each v != O).";
UniqueCollinearQ::usage = "UniqueCollinearQ[graph, vertices] tests whether the listed vertices lie on a unique common line.";
WhiteheadW1Q::usage = "WhiteheadW1Q[graph] tests Whitehead axiom W1 for projective planes: every line has at least three vertices.";
WhiteheadW2Q::usage = "WhiteheadW2Q[graph] tests Whitehead axiom W2: through any two distinct vertices there is exactly one line. This is the geodetic-graph predicate.";
WhiteheadW3Q::usage = "WhiteheadW3Q[graph] tests Whitehead axiom W3: for any four distinct vertices A, B, C, D, if some line through A,B meets some line through C,D then some line through A,C meets some line through B,D. O(|V|^4) -- use on small graphs.";
ProjectivePlaneGraphQ::usage = "ProjectivePlaneGraphQ[graph] tests whether the graph is a projective plane in the synthetic-incidence sense: WhiteheadW1Q && WhiteheadW2Q && WhiteheadW3Q with non-degeneracy (some four vertices, no three collinear).";

(* Scenes *)
InfraScene::usage = "InfraScene[objects, hypotheses] constructs a scene descriptor from symbolic objects and hypotheses (constructions and assertions). Access properties via scene[\"Steps\"], scene[\"Constructions\"], scene[\"Assertions\"], scene[\"DependencyGraph\"].";
FindInfraScene::usage = "FindInfraScene[scene, graph] evaluates all construction steps and returns a list of InfraInstance objects. FindInfraScene[scene, graph, n] evaluates up to n steps. FindInfraScene[scene, graph, <|p -> v, ...|>] starts with pre-fixed bindings. FindInfraScene[scene, graph, n, <|...|>] combines both. Option: \"PruningProbability\" (default 0).";
InfraInstance::usage = "InfraInstance[bindings] wraps a solved binding association from FindInfraScene. Access bindings via instance[[1]][symbol].";
InfraGeometricStep::usage = "InfraGeometricStep[{hyp1, hyp2, ...}] groups hypotheses into a manual construction step. InfraGeometricStep[{hyps...}, \"label\"] adds a label. When used in InfraScene, steps follow the given order instead of auto-computed dependency levels. Points from earlier steps are treated as fixed when later steps execute.";
InfraPoint::usage = "InfraPoint[] represents any vertex. InfraPoint[v] fixes a vertex. InfraPoint[\"Center\"] or InfraPoint[\"Periphery\"] selects from graph center/periphery. InfraPoint[origin, dist] selects vertices at given distance. InfraPoint[n] finds n mutually distant vertices.";
InfraSegment::usage = "InfraSegment[p, q] represents geodesics between p and q. Used in InfraScene hypotheses.";
InfraLine::usage = "InfraLine[p, q] represents maximal geodesic extensions through p and q. InfraLine[path] extends a given path. Used in InfraScene hypotheses.";
InfraSphere::usage = "InfraSphere[center, radius] represents metric spheres at given radius from center. Used in InfraScene hypotheses.";
InfraIntersection::usage = "InfraIntersection[obj1, obj2] represents the vertex set intersection of two geometric objects. Used in InfraScene hypotheses.";
InfraDistance::usage = "InfraDistance[p, q] represents graph distance between p and q. Used in InfraScene assertions.";
InfraSegmentQ::usage = "InfraSegmentQ[s] asserts that s is a valid geodesic segment. Used in InfraScene assertions.";
InfraSphereQ::usage = "InfraSphereQ[c] asserts that c is a valid metric sphere. Used in InfraScene assertions.";
InfraLineQ::usage = "InfraLineQ[s] asserts that s is a maximal geodesic. Used in InfraScene assertions.";
InfraParallelQ::usage = "InfraParallelQ[l1, l2] asserts that two lines are parallel. Used in InfraScene assertions.";
InfraIntersectQ::usage = "InfraIntersectQ[s1, s2] asserts that two sets intersect. Used in InfraScene assertions.";

(* Interactive Viewers *)
InfraSceneViewer::usage = "InfraSceneViewer[scene, graph] creates an interactive visualization of an InfraScene on a graph. InfraSceneViewer[scene, graph, <|p -> v, ...|>] starts with pre-fixed bindings. A step slider controls the current construction step, \"Fix & advance\" pins the current instance and moves to the next step, and \"Reset\" clears all fixed bindings. Aggregate mode shows branch frequency; single-instance mode browses individual instances.";

(* Viewers *)
InfraDiffuseHighlight::usage = "InfraDiffuseHighlight[g, candidates] highlights the union of all candidates in a single shaded color, with edge thickness and opacity proportional to how many candidates pass through each vertex/edge. Renders the multiplicity of a multiconstruction (FindSegment, FindLine, FindSphere with All) as a diffuse, superposition-style picture. InfraDiffuseHighlight[g, candidates, color] overrides the default color. Option \"Cyclic\" -> True closes each candidate into a cycle (for spheres). Options \"OpacityRange\", \"ThicknessRange\", \"PointSizeRange\" tune the diffusion appearance.";
PointViewer::usage = "PointViewer[g] creates an interactive viewer for selecting points in a graph. Controls: Points slider, From (Random/Center/Periphery), Separation (None/Max). PointViewer[g, sym] stores the current selection in sym.";
SegmentViewer::usage = "SegmentViewer[g] creates an interactive visualization for exploring geodesic segments between points in a graph.";
SphereViewer::usage = "SphereViewer[g] creates an interactive visualization for exploring metric spheres centered at a point in a graph.";
