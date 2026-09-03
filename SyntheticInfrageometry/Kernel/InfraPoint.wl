Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findPointPool]
PackageScope[selectFromPointSpace]
PackageScope[infraPointVertices]


(* ===================== InfraPoint wrapper ===================== *)

(* the ATOM of the point ontology: one vertex of the substrate, nothing more.  On a graph whose vertices are lists no guard can separate the atom from a bundle at arity 1, which is why the family layer (InfraSet) and the measure layer (InfraEffectivePoint) are distinct heads *)

(* idempotency: re-wrapping an atom is the identity *)
InfraPoint[ inner_InfraPoint ] := inner


InfraPoint[ v_ ][ "Vertex" ]   := v
InfraPoint[ v_ ][ "First" ]    := v
InfraPoint[ v_ ][ "Vertices" ] := { v }
InfraPoint[ v_ ][ "Mass" ]     := 1

(* an atom is the unit mass at its vertex *)
InfraPoint[ v_ ][ "OccupationCount" ]    := <| v -> 1 |>
InfraPoint[ v_ ][ "OccupationMeasure" ]  := <| v -> 1 |>
InfraPoint[ v_ ][ "Measure" ]            := <| v -> 1 |>
InfraPoint[ v_ ][ "ProbabilityMeasure" ] := <| v -> 1 |>


InfraPoint[ v_ ][ "BallVolumes", g_, rest___ ]            := BallVolumes[ g, v, rest ]
InfraPoint[ v_ ][ "ShellAreas", g_, rest___ ]             := ShellAreas[ g, v, rest ]
InfraPoint[ v_ ][ "LogDifferenceQuotients", g_, rest___ ] := LogDifferenceQuotients @ BallVolumes[ g, v, rest ]
InfraPoint[ v_ ][ "GrowthObservables", g_, rest___ ]      := VolumeGrowthObservables[ g, v, rest ]
InfraPoint[ v_ ][ "Dimension", g_, rest___ ]              := VolumeGrowthObservables[ g, v, rest ][ "BallDimension" ]
InfraPoint[ v_ ][ "ScalarCurvature", g_, rest___ ]        := VolumeGrowthObservables[ g, v, rest ][ "BallScalarCurvature" ]
InfraPoint[ v_ ][ "CurvatureByRadius", g_, rest___ ]      := VolumeGrowthObservables[ g, v, rest ][ "BallCurvatureByRadius" ]

InfraPoint /: BallVolumes[ g_, p_InfraPoint, rest___ ]             := BallVolumes[ g, p[ "Vertex" ], rest ]
InfraPoint /: ShellAreas[ g_, p_InfraPoint, rest___ ]              := ShellAreas[ g, p[ "Vertex" ], rest ]
InfraPoint /: VolumeGrowthObservables[ g_, p_InfraPoint, rest___ ] := VolumeGrowthObservables[ g, p[ "Vertex" ], rest ]


(* ===================== FindInfraPoint ===================== *)

(* "Distance" constrains which points: r fixes the mutual distance exactly, {dMin, dMax} a range, "Max" maximises the minimum pairwise gap, "Spread" breaks the "Max" ties toward minimal variance of the pairwise distances *)

FindInfraPoint::badfrom = "\"From\" specification `1` is not supported by FindInfraPoint. Supported: All, \"Random\", \"Center\", \"Periphery\", {\"Center\", cap}, anchor -> spec, a vertex, a vertex list, InfraPoint, InfraEffectivePoint, InfraSet.";

Options[ FindInfraPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindInfraPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { from = OptionValue[ "From" ], pool,
            dist = OptionValue[ "Distance" ],
            maxCl = OptionValue[ "MaxCliques" ],
            distMatrix, finiteMax, cliques },
    If[ ! fromPointSpecQ[ graph, from ],
      Message[ FindInfraPoint::badfrom, from ]; Return[ $Failed ] ];
    pool = findPointPool[ graph, from ];
    InfraPoint /@ If[ n == 1 || dist === None,
      RandomSample[ pool, UpTo[ n ] ],
      With[ { vertexIndex = Lookup[ AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ], pool ] },
        distMatrix = GraphDistanceMatrix[ graph ][[ vertexIndex, vertexIndex ]];
        finiteMax = Max @ Select[ Flatten @ distMatrix, # < Infinity & ];
        distMatrix = Replace[ distMatrix, Infinity -> finiteMax + 1, { 2 } ];
        With[ { mask = 1 - IdentityMatrix @ Length @ vertexIndex },
          If[ dist === "Max" || dist === "Spread",
            cliques = { };
            Do[
              cliques = FindClique[
                AdjacencyGraph[ pool, UnitStep[ distMatrix - d ] * UnitStep[ finiteMax - distMatrix ] * mask ],
                { n, Length @ pool }, maxCl ];
              If[ cliques =!= { }, Break[ ] ],
              { d, Reverse @ DeleteCases[ Union @@ distMatrix, 0 | _?( # > finiteMax & ) ] } ];
            Which[
              cliques === { }, { },
              dist === "Spread", mostEquidistantSubset[ cliques, distMatrix, pool, n ],
              True, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ],
            With[ { range = Replace[ dist,
                    { d_?NumericQ :> { d, d },
                      { dMin_, dMax_ } :> { dMin, dMax /. Infinity -> finiteMax } } ] },
              cliques = FindClique[
                AdjacencyGraph[ pool, UnitStep[ distMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - distMatrix ] * mask ],
                { Min[ n, Length @ pool ], Length @ pool }, maxCl ];
              If[ cliques === { }, { }, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ]
            ]
          ]
        ]
      ]
    ]
  ]

FindInfraPoint[ graph_Graph, All, opts : OptionsPattern[] ] :=
  FindInfraPoint[ graph, UpTo[ VertexCount[ graph ] ], opts ]

FindInfraPoint[ graph_Graph, n_Integer, opts : OptionsPattern[] ] :=
  With[ { result = FindInfraPoint[ graph, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ] ]

FindInfraPoint[ graph_Graph, opts : OptionsPattern[] ] :=
  With[ { from = OptionValue[ "From" ] },
    If[ ! fromPointSpecQ[ graph, from ],
      Message[ FindInfraPoint::badfrom, from ]; $Failed,
      InfraPoint @ RandomChoice @ findPointPool[ graph, from ] ] ]


(* an unrecognised selector must raise ::badfrom rather than fall through to the whole vertex pool, which reads as a legitimate random draw *)

fromPointSpecQ[ graph_Graph, spec_ ] :=
  MatchQ[ spec, All | "Random" | "Center" | "Periphery" | { "Center", _Integer | Infinity }
                | _InfraPoint | _InfraEffectivePoint | _InfraSet | _Rule | _List ] ||
  MemberQ[ VertexList @ graph, spec ]


findPointPool[ graph_Graph, "Center" ]    := GraphCenter[ graph ]
findPointPool[ graph_Graph, "Periphery" ] := GraphPeriphery[ graph ]

(* orbit of c |-> NeighborhoodGraph[c, GraphCenter[c]], stopping at a fixed point or when the centre-neighbourhood first disconnects *)
findPointPool[ graph_Graph ? ConnectedGraphQ, { "Center", cap : ( _Integer | Infinity ) } ] :=
  With[ { final = NestWhile[ NeighborhoodGraph[ #, GraphCenter[ # ] ] &, graph,
            ConnectedGraphQ[ #2 ] && VertexCount[ #1 ] != VertexCount[ #2 ] &, 2, cap ] },
    If[ ConnectedGraphQ[ final ], GraphCenter[ final ], VertexList[ final ] ] ]

findPointPool[ graph_Graph, { "Center", _ } ] := VertexList[ graph ]

findPointPool[ graph_Graph, _String ]     := VertexList[ graph ]

findPointPool[ graph_Graph, InfraPoint[ v_ ] ] := { v }
findPointPool[ graph_Graph, InfraEffectivePoint[ m_Association ] ] := Keys @ m
findPointPool[ graph_Graph, InfraSet[ vs_List ] ] := vs

findPointPool[ graph_Graph, ( origin_ -> spec_ ) ] :=
  With[ { anchors = infraSpread[ origin ],
          vertexIndex = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount[ graph ] ] },
    { anchorDists = Association[ # -> GraphDistance[ graph, # ] & /@ anchors ] },
    Select[ VertexList[ graph ],
      v |-> AllTrue[ anchors, a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ] ] ]
  ]

findPointPool[ graph_Graph, v_ ] /; MemberQ[ VertexList[ graph ], v ] := { v }
findPointPool[ graph_Graph, list : { __InfraPoint } ] := #[[ 1 ]] & /@ list
findPointPool[ graph_Graph, list_List ] := list
findPointPool[ graph_Graph, _ ]         := VertexList[ graph ]


anchorDistMatchQ[ allDists_List, idx_Integer, d_?NumericQ ]                  := allDists[[ idx ]] == d
anchorDistMatchQ[ allDists_List, idx_Integer, { lo_?NumericQ, hi_?NumericQ } ] := lo <= allDists[[ idx ]] <= hi
anchorDistMatchQ[ allDists_List, idx_Integer, "Max" ]                        :=
  allDists[[ idx ]] == Max @ Select[ allDists, # < Infinity & ]


(* among the max-min-gap cliques, the n-subset of minimal variance of pairwise distances *)

mostEquidistantSubset[ cliques_List, distMatrix_, pool_List, n_Integer ] :=
  With[ { idx = AssociationThread[ pool -> Range @ Length @ pool ],
          subsets = DeleteDuplicates[ Sort /@ Catenate[ Subsets[ #, { n } ] & /@ cliques ] ] },
    If[ n < 3,
      First @ subsets,
      First @ MinimalBy[ subsets,
        s |-> Variance[ distMatrix[[ idx @ #[[ 1 ]], idx @ #[[ 2 ]] ]] & /@ Subsets[ s, { 2 } ] ] ] ]
  ]


(* ===================== FindInfraMidpoint ===================== *)

(* the vertices at the index closest to the centre index (n + 1)/2: an odd distance gives two of them, an even one a single vertex *)

Options[ FindInfraMidpoint ] = { Method -> "Metric", "Tolerance" -> 0 };

(* occupation of the central index band, vertex -> mass: a walk contributes 1 per band vertex, a geodesic-DAG atom reads the band off its layers weighted by geodesic occupation *)

indexBandMasses[ frac_, tol_ ][ dag_Graph ] :=
  With[ { layers = dagLayers[ dag ] },
    If[ Length @ layers === 0, <||>,
      With[ { occ = GeodesicOccupation[ dag ], len = Max[ 0, Values @ layers ] },
        { offs = Abs[ # - frac * len ] & /@ layers },
        KeyTake[ occ, Keys @ Select[ offs, # <= Min[ Values @ offs ] + tol & ] ] ] ] ]

indexBandMasses[ frac_, tol_ ][ walk_List ] :=
  With[ { offsets = Abs[ Range[ Length @ walk ] - ( 1 + frac ( Length @ walk - 1 ) ) ] },
    Counts @ Pick[ walk, Thread[ offsets <= Min[ offsets ] + tol ], True ] ]

FindInfraMidpoint[ graph_Graph, InfraSegment[ dag_Graph ], opts : OptionsPattern[] ] :=
  If[ methodName @ OptionValue[ FindInfraMidpoint, { opts }, Method ] === "Metric",
    InfraEffectivePoint @ indexBandMasses[ 1/2, OptionValue[ FindInfraMidpoint, { opts }, "Tolerance" ] ][ dag ],
    FindInfraMidpoint[ graph, InfraSegment[ dagGeodesics[ dag ] ], opts ] ]

FindInfraMidpoint[ graph_Graph, seg_InfraSegment, opts : OptionsPattern[] ] :=
  With[ { method = methodName @ OptionValue[ Method ], tol = OptionValue[ "Tolerance" ] },
    Switch[ method,
      "Metric",
        InfraEffectivePoint @ Merge[ indexBandMasses[ 1/2, tol ] /@ First @ seg, Total ],
      "Embedding",
        (* closest vertex to the coord-space midpoint of the endpoints *)
        With[ { walks = First[ seg ], embOpts = parseEmbeddingMethod @ OptionValue[ Method ] },
          { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
            vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
          { target = ( coords[[ vertexIndex[ First @ First @ walks ] ]] +
                       coords[[ vertexIndex[ Last @ First @ walks ] ]] ) / 2,
            pool = If[ embOpts[ "Pool" ] === "AllPaths",
                     VertexList[ graph ], DeleteDuplicates @ Catenate @ walks ] },
          InfraEffectivePoint[ <| First @
            SortBy[ pool, v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ] -> 1 |> ] ]
    ]
  ]

FindInfraMidpoint[ graph_Graph, walk_List, opts : OptionsPattern[] ] /; Length[ walk ] >= 2 :=
  FindInfraMidpoint[ graph, InfraSegment[ { walk } ], opts ]

FindInfraMidpoint[ graph_Graph, p1_, p2_, opts : OptionsPattern[] ] :=
  FindInfraMidpoint[ graph, FindInfraSegment[ graph, p1, p2, All ], opts ]


(* ===================== FindInfraGoldenSection ===================== *)

(* AB/AS == AS/SB, i.e. AS == AB/phi: the vertex at the index closest to 1 + (n - 1)/phi.  The golden index is irrational, so it is a single vertex per walk *)

Options[ FindInfraGoldenSection ] = { Method -> "Metric", "Tolerance" -> 0 };

FindInfraGoldenSection[ graph_Graph, InfraSegment[ dag_Graph ], opts : OptionsPattern[] ] :=
  If[ methodName @ OptionValue[ FindInfraGoldenSection, { opts }, Method ] === "Metric",
    InfraEffectivePoint @ indexBandMasses[ N[ 1 / GoldenRatio ],
      OptionValue[ FindInfraGoldenSection, { opts }, "Tolerance" ] ][ dag ],
    FindInfraGoldenSection[ graph, InfraSegment[ dagGeodesics[ dag ] ], opts ] ]

FindInfraGoldenSection[ graph_Graph, seg_InfraSegment, opts : OptionsPattern[] ] :=
  With[ { method = methodName @ OptionValue[ Method ], tol = OptionValue[ "Tolerance" ] },
    Switch[ method,
      "Metric",
        InfraEffectivePoint @ Merge[ indexBandMasses[ N[ 1 / GoldenRatio ], tol ] /@ First @ seg, Total ],
      "Embedding",
        (* closest vertex to the coord-space golden point p1 + (p2 - p1)/phi *)
        With[ { walks = First[ seg ], embOpts = parseEmbeddingMethod @ OptionValue[ Method ] },
          { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
            vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
          { target = coords[[ vertexIndex[ First @ First @ walks ] ]] +
                     ( coords[[ vertexIndex[ Last @ First @ walks ] ]] -
                       coords[[ vertexIndex[ First @ First @ walks ] ]] ) / N[ GoldenRatio ],
            pool = If[ embOpts[ "Pool" ] === "AllPaths",
                     VertexList[ graph ], DeleteDuplicates @ Catenate @ walks ] },
          InfraEffectivePoint[ <| First @
            SortBy[ pool, v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ] -> 1 |> ] ]
    ]
  ]

FindInfraGoldenSection[ graph_Graph, walk_List, opts : OptionsPattern[] ] /; Length[ walk ] >= 3 :=
  FindInfraGoldenSection[ graph, InfraSegment[ { walk } ], opts ]

FindInfraGoldenSection[ graph_Graph, p1_, p2_, opts : OptionsPattern[] ] :=
  FindInfraGoldenSection[ graph, FindInfraSegment[ graph, p1, p2, All ], opts ]


(* ===================== FindInfraReflection ===================== *)

(* y with BetweennessQ[x, a, y] and d(a, y) = d(a, x): the geodesic continuation of x past a at the same distance *)

FindInfraReflection[ graph_Graph, x_, a_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  spreadFind[ InfraPoint, count,
    { x0, a0 } |-> With[ { r = GraphDistance[ graph, a0, x0 ] },
      If[ r === Infinity, {},
        (* Localize: reflection lives in B(a, r), straddle-paths stay in B(a, 2 r). *)
        With[ { localG = NeighborhoodGraph[ graph, a0, 2 r ] },
          Select[ VertexList[ localG ],
            y |-> BetweennessQ[ localG, x0, a0, y ] && GraphDistance[ localG, a0, y ] === r ] ]
      ]
    ], x, a ]


(* ===================== CompleteInfraEquilateralTriangle ===================== *)

(* Euclid I.1: c with d(p1, c) = d(p2, c) = d(p1, p2), the intersection of the two spheres *)

Options[ CompleteInfraEquilateralTriangle ] = { Method -> "Metric" };

CompleteInfraEquilateralTriangle[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  spreadFind[ InfraPoint, count,
    { q1, q2 } |-> With[ { r = GraphDistance[ graph, q1, q2 ] },
      If[ r === Infinity, {},
        Intersection[
          Select[ VertexList[ graph ], GraphDistance[ graph, q1, # ] == r & ],
          Select[ VertexList[ graph ], GraphDistance[ graph, q2, # ] == r & ] ]
      ]
    ], p1, p2 ]


(* ===================== FindInfraCommonPoint ===================== *)

(* the intersection of the listed lines *)

FindInfraCommonPoint[ graph_Graph, lines_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { vs = If[ Length[ lines ] == 0, {},
        Apply[ Intersection, linePointSet /@ lines ] ] },
    bundleTake[ InfraPoint, vs, count ]
  ]


(* ===================== FindClosestInfraPoint ===================== *)

(* metric argmin: the vertices of line at minimum distance from point, not the Euclid I.12 foot FindInfraPerpendicular runs *)

FindClosestInfraPoint[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  spreadFind[ InfraPoint, count,
    { line0, point0 } |-> MinimalBy[ line0, GraphDistance[ graph, point0, # ] & ], line, point ]


(* ===================== SelectInfraPoint ===================== *)


SelectInfraPoint::badfrom = "\"From\" specification `1` is not supported by SelectInfraPoint. Supported: All, \"Random\", \"Center\", \"Periphery\", anchor -> spec, a vertex, a vertex list, InfraPoint, InfraEffectivePoint, InfraSet.";

Options[ SelectInfraPoint ] = { "From" -> All, "Distance" -> None, "MaxCliques" -> All };

SelectInfraPoint[ graph_Graph, vertices_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ _InfraPoint ] ] :=
  With[ { from = OptionValue[ "From" ] },
    If[ ! fromPointSpecQ[ graph, from ],
      Message[ SelectInfraPoint::badfrom, from ]; $Failed,
      InfraPoint /@ selectFromPointSpace[ graph, vertices, n, from,
        OptionValue[ "Distance" ], OptionValue[ "MaxCliques" ] ] ] ]

SelectInfraPoint[ graph_Graph, vertices_List, All, opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ _InfraPoint ] ] :=
  SelectInfraPoint[ graph, vertices, UpTo[ Length[ vertices ] ], opts ]

SelectInfraPoint[ graph_Graph, vertices_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ _InfraPoint ] ] :=
  With[ { result = SelectInfraPoint[ graph, vertices, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectInfraPoint[ graph_Graph, list : { __InfraPoint },
                  countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  SelectInfraPoint[ graph, #[[ 1 ]] & /@ list, countSpec, opts ]

SelectInfraPoint[ graph_Graph,
                  bundle : ( InfraBall | InfraShell | InfraEllipticShell | InfraPlane | InfraSet | InfraObject | InfraCircle | InfraEllipse )[ _List ] | _InfraEffectivePoint,
                  countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  SelectInfraPoint[ graph, infraVertexSet[ bundle ], countSpec, opts ]

SelectInfraPoint[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectInfraPoint[ graph, #, countSpec, opts ] &


selectFromPointSpace[ _Graph, vertices_List, _Integer, _, _, _ ] /; Length[ vertices ] <= 1 := vertices

selectFromPointSpace[ graph_Graph, vertices_List, nMax_Integer,
                      fromSpec_, distSpec_, maxCl_ ] :=
  Module[ { vIdx, subMatrix, poolIdx, pool, poolSubMatrix, finiteMax, range,
            auxiliaryGraph, cliques, thresholds, n },
    vIdx = Lookup[ AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ], vertices ];
    subMatrix = GraphDistanceMatrix[ graph ][[ vIdx, vIdx ]];
    poolIdx = pointPoolPositions[ graph, vertices, fromSpec, subMatrix ];
    If[ poolIdx === { }, Return[ { } ] ];
    pool = vertices[[ poolIdx ]];
    n = Min[ nMax, Length[ pool ] ];
    If[ distSpec === None || n <= 1,
      Return[ If[ n >= Length[ pool ], pool, RandomSample[ pool, n ] ] ] ];
    poolSubMatrix = subMatrix[[ poolIdx, poolIdx ]];
    finiteMax = Replace[ Max @ Select[ Flatten @ poolSubMatrix, # < Infinity & ],
      _?( ! NumericQ @ # & ) -> 0 ];
    poolSubMatrix = Replace[ poolSubMatrix, Infinity -> finiteMax + 1, { 2 } ];
    Which[
      distSpec === "Max" || distSpec === "Spread",
        thresholds = Reverse @ DeleteCases[ Union @@ poolSubMatrix, 0 | _?( # > finiteMax & ) ];
        cliques = { };
        Do[
          auxiliaryGraph = AdjacencyGraph[ pool,
            UnitStep[ poolSubMatrix - d ] * UnitStep[ finiteMax - poolSubMatrix ]
              * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
          cliques = FindClique[ auxiliaryGraph, { n, VertexCount[ auxiliaryGraph ] }, maxCl ];
          If[ cliques =!= { }, Break[ ] ],
          { d, thresholds } ];
        Which[
          cliques === { }, { },
          distSpec === "Spread", mostEquidistantSubset[ cliques, poolSubMatrix, pool, n ],
          True, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ],
      True,
        range = Replace[ distSpec,
          { d_?NumericQ                  :> { d, d },
            { dMin_?NumericQ, Infinity } :> { dMin, finiteMax },
            { dMin_?NumericQ, dMax_?NumericQ } :> { dMin, dMax },
            _ :> { 0, finiteMax } } ];
        auxiliaryGraph = AdjacencyGraph[ pool,
          UnitStep[ poolSubMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - poolSubMatrix ]
            * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
        cliques = FindClique[ auxiliaryGraph,
          { Min[ n, VertexCount[ auxiliaryGraph ] ], VertexCount[ auxiliaryGraph ] }, maxCl ];
        If[ cliques === { }, { }, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ]
    ]
  ]


pointPoolPositions[ _Graph, vertices_List, All, _ ] := Range @ Length @ vertices

pointPoolPositions[ _, _, "Center", subMatrix_ ] :=
  With[ { scores = Max /@ subMatrix },
    Flatten @ Position[ scores, Min @ scores, { 1 }, Heads -> False ] ]

pointPoolPositions[ _, _, "Periphery", subMatrix_ ] :=
  With[ { scores = Max /@ subMatrix },
    Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ] ]

pointPoolPositions[ graph_Graph, vertices_List, ( anchor_ -> spec_ ), _ ] :=
  With[ { anchors = infraSpread[ anchor ],
          vertexIndex = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount[ graph ] ] },
    { anchorDists = Association[ # -> GraphDistance[ graph, # ] & /@ anchors ] },
    Flatten @ Position[ vertices,
      v_ /; AllTrue[ anchors, a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ] ],
      { 1 }, Heads -> False ]
  ]

pointPoolPositions[ _, vertices_List, InfraSet[ reps_List ], _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ reps, { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, list : { __InfraPoint }, _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ ( #[[ 1 ]] & /@ list ), { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, v_, _ ] /; MemberQ[ vertices, v ] :=
  { First @ FirstPosition[ vertices, v ] }

pointPoolPositions[ _, vertices_List, list_List, _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ list, { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, _, _ ] := Range @ Length @ vertices


(* ===================== InfraReachableQ ===================== *)
(* single multi-source BFS via VertexComponent: visits only the components touched by p1's realisations *)

InfraReachableQ[ graph_Graph, p1_, p2_ ] :=
  IntersectingQ[ VertexComponent[ graph, infraPointVertices @ p1 ], infraPointVertices @ p2 ]

infraPointVertices[ InfraPoint[ v_ ] ]        := { v }
infraPointVertices[ InfraSet[ vs_List ] ]     := vs
infraPointVertices[ InfraEffectivePoint[ m_Association ] ] := Keys @ m
infraPointVertices[ list : { __InfraPoint } ] := #[[ 1 ]] & /@ list
infraPointVertices[ list_List ]               := list
infraPointVertices[ v_ ]                     := { v }


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraPoint[ ] ] :=
  VertexList @ graph

dispatchConstruction[ graph_Graph, InfraPoint[ v_ ] ] /; MemberQ[ VertexList @ graph, v ] :=
  { v }

dispatchConstruction[ graph_Graph, InfraSet[ vs_List ] ] /;
    vs =!= { } && SubsetQ[ VertexList @ graph, vs ] :=
  vs

dispatchConstruction[ graph_Graph, list : { __InfraPoint } ] /;
    SubsetQ[ VertexList @ graph, #[[ 1 ]] & /@ list ] :=
  #[[ 1 ]] & /@ list

dispatchConstruction[ graph_Graph, InfraPoint[ pool_String ] ] :=
  Switch[ pool,
    "Center",    GraphCenter[ graph ],
    "Periphery", GraphPeriphery[ graph ],
    _,           VertexList @ graph
  ]

dispatchConstruction[ graph_Graph, InfraPoint[ origin_, dist_ ] ] /; ! MatchQ[ dist, _Rule ] :=
  DeleteDuplicates @ Flatten[ Map[
    o |-> Select[ VertexList @ graph, v |-> GraphDistance[ graph, o, v ] == dist ],
    If[ StringQ @ origin,
      Switch[ origin,
        "Center",    GraphCenter[ graph ],
        "Periphery", GraphPeriphery[ graph ],
        _,           VertexList @ graph ],
      If[ MemberQ[ VertexList @ graph, origin ], { origin }, origin ] ] ], 1 ]

dispatchConstruction[ graph_Graph, InfraPoint[ n_Integer, opts___Rule ] ] :=
  Module[ { dist, distMatrix, finiteMax, dMin, dMax, auxGraph, cliques },
    dist          = "Distance" /. { opts } /. "Distance" -> "Max";
    distMatrix    = GraphDistanceMatrix @ graph;
    finiteMax     = Max @ Select[ Flatten @ distMatrix, # < Infinity & ];
    { dMin, dMax } = Switch[ dist,
      "Max", { finiteMax, finiteMax },
      _List, dist /. Infinity -> finiteMax,
      _,     { dist, finiteMax } ];
    auxGraph = Graph[ VertexList @ graph,
      UndirectedEdge @@@ Select[ Subsets[ VertexList @ graph, { 2 } ],
        pair |-> With[ { d = GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] },
          dMin <= d <= dMax ] ] ];
    cliques = Select[ FindClique[ auxGraph, { n, VertexCount @ auxGraph }, All ],
      Length @ # >= n & ];
    If[ cliques === {}, {}, RandomSample[ #, n ] & /@ cliques ]
  ]
