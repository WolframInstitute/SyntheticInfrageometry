Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findPointPool]
PackageScope[selectFromPointSpace]


(* ===================== InfraPoint wrapper ===================== *)

(* InfraPoint[{v1, ...}] is the unweighted form (each vertex mass 1).
   InfraPoint[{v1, ...}, {w1, ...}] carries explicit per-vertex mass.  The
   canonical form has a duplicate-free support with positive masses; masses
   add on aggregation (a commutative monoid), and the all-ones weighted form
   collapses back to the bare 1-arg form. *)

InfraPoint[ reps_List ] /; ! FreeQ[ reps, _InfraPoint ] :=
  mergeInfraPoints @ Replace[ reps, {
      InfraPoint[ vs_List, ws_List ] :> Thread[ vs -> ws ],
      InfraPoint[ vs_List ]          :> Thread[ vs -> 1 ],
      v_                             :> { v -> 1 } }, { 1 } ]

InfraPoint[ reps_List ] /; FreeQ[ reps, _InfraPoint ] && ! DuplicateFreeQ[ reps ] :=
  With[ { m = Counts @ reps }, InfraPoint[ Keys @ m, Values @ m ] ]

InfraPoint[ verts_List, weights_List ] /; ! DuplicateFreeQ[ verts ] :=
  With[ { m = Merge[ Thread[ verts -> weights ], Total ] }, InfraPoint[ Keys @ m, Values @ m ] ]

InfraPoint[ verts_List, weights_List ] /;
    DuplicateFreeQ[ verts ] && weights === ConstantArray[ 1, Length @ verts ] :=
  InfraPoint[ verts ]

mergeInfraPoints[ pairs_List ] :=
  With[ { m = Merge[ Flatten[ pairs, 1 ], Total ] }, InfraPoint[ Keys @ m, Values @ m ] ]

InfraPoint[ verts_List ][ "Support" ]               := verts
InfraPoint[ verts_List, _List ][ "Support" ]        := verts
InfraPoint[ verts_List ][ "Weights" ]               := ConstantArray[ 1, Length @ verts ]
InfraPoint[ verts_List, weights_List ][ "Weights" ] := weights
InfraPoint[ verts_List ][ "Mass" ]                  := Length @ verts
InfraPoint[ verts_List, weights_List ][ "Mass" ]    := Total @ weights
InfraPoint[ verts_List, ___ ][ "First" ]            := First @ verts

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraPoint[ verts_List, w___ ][ "OccupationCount" ] := infraVertexMultiset[ InfraPoint[ verts, w ] ]
InfraPoint[ verts_List, w___ ][ "OccupationMeasure" ] := InfraMeasure[ InfraPoint[ verts, w ] ]
InfraPoint[ verts_List, w___ ][ "Measure" ] := InfraMeasure[ InfraPoint[ verts, w ] ]
InfraPoint[ verts_List, w___ ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraPoint[ verts, w ], Method -> "Probability" ]

(* synthetic-invariant accessors (Infrageometry primitives over the support, one result
   per support vertex; the graph is passed in since the wrapper holds no graph).  Extra
   args forward verbatim -- e.g. p["BallVolumes", g, {0, R}], p["Dimension", g, {1, 5}].
   The dimension / curvature readouts project the Ball* keys of VolumeGrowthObservables. *)
InfraPoint[ verts_List, ___ ][ "BallVolumes", g_, rest___ ]            := BallVolumes[ g, verts, rest ]
InfraPoint[ verts_List, ___ ][ "ShellAreas", g_, rest___ ]             := ShellAreas[ g, verts, rest ]
InfraPoint[ verts_List, ___ ][ "LogDifferenceQuotients", g_, rest___ ] := LogDifferenceQuotients /@ BallVolumes[ g, verts, rest ]
InfraPoint[ verts_List, ___ ][ "GrowthObservables", g_, rest___ ]      := VolumeGrowthObservables[ g, verts, rest ]
InfraPoint[ verts_List, ___ ][ "Dimension", g_, rest___ ]              := ( #[ "BallDimension" ] & ) /@ VolumeGrowthObservables[ g, verts, rest ]
InfraPoint[ verts_List, ___ ][ "ScalarCurvature", g_, rest___ ]        := ( #[ "BallScalarCurvature" ] & ) /@ VolumeGrowthObservables[ g, verts, rest ]
InfraPoint[ verts_List, ___ ][ "CurvatureByRadius", g_, rest___ ]      := ( #[ "BallCurvatureByRadius" ] & ) /@ VolumeGrowthObservables[ g, verts, rest ]

(* the same invariants the other way round: the Infrageometry primitives accept an
   InfraPoint directly (via its support), so BallVolumes[g, p] == p["BallVolumes", g]. *)
InfraPoint /: BallVolumes[ g_, p_InfraPoint, rest___ ]              := BallVolumes[ g, p[ "Support" ], rest ]
InfraPoint /: ShellAreas[ g_, p_InfraPoint, rest___ ]              := ShellAreas[ g, p[ "Support" ], rest ]
InfraPoint /: VolumeGrowthObservables[ g_, p_InfraPoint, rest___ ] := VolumeGrowthObservables[ g, p[ "Support" ], rest ]

(* ===================== FindInfraPoint ===================== *)

(* FindInfraPoint[g, n] returns n unary InfraPoint[{v}] wrappers.  With
   "Distance" -> r the n vertices are mutually at exactly distance r;
   with {dMin, dMax} mutually within that range; with "Max" mutually as far
   apart as possible (maximal minimum pairwise gap, ties broken randomly);
   with "Spread" the same maximal gap but ties broken toward the most
   equidistant set (minimal variance of pairwise distances).  "From" restricts
   the candidate pool. *)

Options[ FindInfraPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindInfraPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { pool = findPointPool[ graph, OptionValue[ "From" ] ],
            dist = OptionValue[ "Distance" ],
            maxCl = OptionValue[ "MaxCliques" ],
            distMatrix, finiteMax, cliques },
    InfraPoint[ { # } ] & /@ If[ n == 1 || dist === None,
      RandomSample[ pool, UpTo[ n ] ],
      With[ { vertexIndex = Lookup[ AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ], pool ] },
        distMatrix = GraphDistanceMatrix[ graph ][[ vertexIndex, vertexIndex ]];
        finiteMax = Max @ Select[ Flatten @ distMatrix, # < Infinity & ];
        distMatrix = Replace[ distMatrix, Infinity -> finiteMax + 1, { 2 } ];
        With[ { mask = 1 - IdentityMatrix @ Length @ vertexIndex },
          If[ dist === "Max" || dist === "Spread",
            cliques = {};
            Do[
              cliques = FindClique[
                AdjacencyGraph[ pool, UnitStep[ distMatrix - d ] * UnitStep[ finiteMax - distMatrix ] * mask ],
                { n, Length @ pool }, maxCl ];
              If[ cliques =!= {}, Break[] ],
              { d, Reverse @ DeleteCases[ Union @@ distMatrix, 0 | _?( # > finiteMax & ) ] } ];
            Which[
              cliques === {}, {},
              dist === "Spread", mostEquidistantSubset[ cliques, distMatrix, pool, n ],
              True, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ],
            With[ { range = Replace[ dist,
                    { d_?NumericQ :> { d, d },
                      { dMin_, dMax_ } :> { dMin, dMax /. Infinity -> finiteMax } } ] },
              cliques = FindClique[
                AdjacencyGraph[ pool, UnitStep[ distMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - distMatrix ] * mask ],
                { Min[ n, Length @ pool ], Length @ pool }, maxCl ];
              If[ cliques === {}, {}, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ]
            ]
          ]
        ]
      ]
    ]
  ]

FindInfraPoint[ graph_Graph, All, opts : OptionsPattern[] ] :=
  FindInfraPoint[ graph, UpTo[ VertexCount[ graph ] ], opts ]

FindInfraPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindInfraPoint[ graph, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ] ]


(* "From" option dispatch -- vertex pool to draw points from. *)

findPointPool[ graph_Graph, "Center" ]    := GraphCenter[ graph ]
findPointPool[ graph_Graph, "Periphery" ] := GraphPeriphery[ graph ]

(* iterated centre: orbit of c |-> NeighborhoodGraph[c, GraphCenter[c]] for up to
   cap steps (Infinity = uncapped), stopping at a fixed point (pool = its centre)
   or when the centre-neighbourhood first disconnects (pool = that whole region). *)
findPointPool[ graph_Graph ? ConnectedGraphQ, { "Center", cap : ( _Integer | Infinity ) } ] :=
  With[ { final = NestWhile[ NeighborhoodGraph[ #, GraphCenter[ # ] ] &, graph,
            ConnectedGraphQ[ #2 ] && VertexCount[ #1 ] != VertexCount[ #2 ] &, 2, cap ] },
    If[ ConnectedGraphQ[ final ], GraphCenter[ final ], VertexList[ final ] ] ]

findPointPool[ graph_Graph, { "Center", _ } ] := VertexList[ graph ]

findPointPool[ graph_Graph, _String ]     := VertexList[ graph ]

findPointPool[ graph_Graph, InfraPoint[ reps_List ] ] := reps

findPointPool[ graph_Graph, ( origin_ -> spec_ ) ] :=
  With[ { anchors = infraSpread[ origin ],
          vertexIndex = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount[ graph ] ] },
    With[ { anchorDists = Association[ # -> GraphDistance[ graph, # ] & /@ anchors ] },
      Select[ VertexList[ graph ],
        v |-> AllTrue[ anchors, a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ] ] ]
    ]
  ]

findPointPool[ graph_Graph, v_ ] /; MemberQ[ VertexList[ graph ], v ] := { v }
findPointPool[ graph_Graph, list_List ] /; AllTrue[ list, MatchQ[ InfraPoint[ { _ } ] ] ] := First /@ list
findPointPool[ graph_Graph, list_List ] := list
findPointPool[ graph_Graph, _ ]         := VertexList[ graph ]


anchorDistMatchQ[ allDists_List, idx_Integer, d_?NumericQ ]                  := allDists[[ idx ]] == d
anchorDistMatchQ[ allDists_List, idx_Integer, { lo_?NumericQ, hi_?NumericQ } ] := lo <= allDists[[ idx ]] <= hi
anchorDistMatchQ[ allDists_List, idx_Integer, "Max" ]                        :=
  allDists[[ idx ]] == Max @ Select[ allDists, # < Infinity & ]


(* Among the max-min-gap cliques, the n-subset whose pairwise distances are
   most equal -- minimal variance of pairwise distances, the most
   equidistantly spread configuration.  distMatrix is the pool x pool distance
   submatrix indexed by position in pool. *)

mostEquidistantSubset[ cliques_List, distMatrix_, pool_List, n_Integer ] :=
  With[ { idx = AssociationThread[ pool -> Range @ Length @ pool ],
          subsets = DeleteDuplicates[ Sort /@ Catenate[ Subsets[ #, { n } ] & /@ cliques ] ] },
    If[ n < 3,
      First @ subsets,
      First @ MinimalBy[ subsets,
        s |-> Variance[ distMatrix[[ idx @ #[[ 1 ]], idx @ #[[ 2 ]] ]] & /@ Subsets[ s, { 2 } ] ] ] ]
  ]


(* ===================== FindInfraMidpoint ===================== *)

(* Midpoint of a walk: the vertices at the index/indices closest to the centre
   index (n + 1)/2.  Even-length walks (odd distance) give two equidistant
   vertices -- a mesopoint; odd-length walks give a single vertex.  Always
   non-empty.  Across the walks of the segment the closest-index vertices are
   unioned into one InfraPoint.  "Tolerance" widens the band beyond the closest
   index by that many graph-distance units. *)

Options[ FindInfraMidpoint ] = { Method -> "Metric", "Tolerance" -> 0 };

FindInfraMidpoint[ graph_Graph, InfraSegment[ dag_Graph ], opts : OptionsPattern[] ] :=
  FindInfraMidpoint[ graph, InfraSegment[ dagGeodesics[ dag ] ], opts ]

FindInfraMidpoint[ graph_Graph, seg_InfraSegment, opts : OptionsPattern[] ] :=
  With[ { method = methodName @ OptionValue[ Method ], tol = OptionValue[ "Tolerance" ] },
    Switch[ method,
      "Metric",
        InfraPoint[ DeleteDuplicates @ Flatten[
          Map[
            walk |-> With[ { offsets = Abs[ Range[ Length[ walk ] ] - ( Length[ walk ] + 1 ) / 2 ] },
              Pick[ walk, Thread[ offsets <= Min[ offsets ] + tol ], True ] ],
            First[ seg ] ], 1 ] ],
      "Embedding",
        (* closest vertex to the coord-space midpoint of the endpoints; pool
           "ShortestPaths" = vertices of the supplied walks, "AllPaths" = all *)
        With[ { walks = First[ seg ], embOpts = parseEmbeddingMethod @ OptionValue[ Method ] },
          { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
            vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
          { target = ( coords[[ vertexIndex[ First @ First @ walks ] ]] +
                       coords[[ vertexIndex[ Last @ First @ walks ] ]] ) / 2,
            pool = If[ embOpts[ "Pool" ] === "AllPaths",
                     VertexList[ graph ], DeleteDuplicates @ Catenate @ walks ] },
          InfraPoint[ { First @
            SortBy[ pool, v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ] } ] ]
    ]
  ]

FindInfraMidpoint[ graph_Graph, walk_List, opts : OptionsPattern[] ] /; Length[ walk ] >= 2 :=
  FindInfraMidpoint[ graph, InfraSegment[ { walk } ], opts ]

FindInfraMidpoint[ graph_Graph, p1_, p2_, opts : OptionsPattern[] ] :=
  FindInfraMidpoint[ graph,
    InfraSegment[ segReps @ FindInfraSegment[ graph, p1, p2, All ] ], opts ]


(* ===================== FindInfraGoldenSection ===================== *)

(* Golden-section vertex S on segment AB: AB/AS == AS/SB, i.e. AS == AB/phi.
   The vertex at the index closest to the golden index 1 + (n - 1)/phi (n =
   walk length); the golden index is irrational, so it is a single vertex per
   walk -- always a single point.  Across the walks of the segment the
   closest-index vertices are unioned into one InfraPoint.  "Tolerance" widens
   the band beyond the closest index by that many graph-distance units. *)

Options[ FindInfraGoldenSection ] = { Method -> "Metric", "Tolerance" -> 0 };

FindInfraGoldenSection[ graph_Graph, InfraSegment[ dag_Graph ], opts : OptionsPattern[] ] :=
  FindInfraGoldenSection[ graph, InfraSegment[ dagGeodesics[ dag ] ], opts ]

FindInfraGoldenSection[ graph_Graph, seg_InfraSegment, opts : OptionsPattern[] ] :=
  With[ { method = methodName @ OptionValue[ Method ], tol = OptionValue[ "Tolerance" ] },
    Switch[ method,
      "Metric",
        InfraPoint[ DeleteDuplicates @ Flatten[
          Map[
            walk |-> With[ { offsets =
                  Abs[ Range[ Length[ walk ] ] - N[ 1 + ( Length[ walk ] - 1 ) / GoldenRatio ] ] },
              Pick[ walk, Thread[ offsets <= Min[ offsets ] + tol ], True ] ],
            First[ seg ] ], 1 ] ],
      "Embedding",
        (* closest vertex to the coord-space golden point p1 + (p2 - p1)/phi;
           pool as in FindInfraMidpoint *)
        With[ { walks = First[ seg ], embOpts = parseEmbeddingMethod @ OptionValue[ Method ] },
          { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
            vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
          { target = coords[[ vertexIndex[ First @ First @ walks ] ]] +
                     ( coords[[ vertexIndex[ Last @ First @ walks ] ]] -
                       coords[[ vertexIndex[ First @ First @ walks ] ]] ) / N[ GoldenRatio ],
            pool = If[ embOpts[ "Pool" ] === "AllPaths",
                     VertexList[ graph ], DeleteDuplicates @ Catenate @ walks ] },
          InfraPoint[ { First @
            SortBy[ pool, v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ] } ] ]
    ]
  ]

FindInfraGoldenSection[ graph_Graph, walk_List, opts : OptionsPattern[] ] /; Length[ walk ] >= 3 :=
  FindInfraGoldenSection[ graph, InfraSegment[ { walk } ], opts ]

FindInfraGoldenSection[ graph_Graph, p1_, p2_, opts : OptionsPattern[] ] :=
  FindInfraGoldenSection[ graph,
    InfraSegment[ segReps @ FindInfraSegment[ graph, p1, p2, All ] ], opts ]


(* ===================== FindInfraReflection ===================== *)

(* Reflection of x through a: vertex y with BetweennessQ[x, a, y] and d(a, y) = d(a, x).
   On a graph this is the geodesic continuation of x past a at the same distance. *)

FindInfraReflection[ graph_Graph, x_, a_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
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

(* Apex of an equilateral triangle on p1, p2 (Euclid I.1): vertex c with
   d(p1, c) = d(p2, c) = d(p1, p2) -- the intersection of the two spheres. *)

Options[ CompleteInfraEquilateralTriangle ] = { Method -> "Metric" };

CompleteInfraEquilateralTriangle[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  spreadFind[ InfraPoint, count,
    { q1, q2 } |-> With[ { r = GraphDistance[ graph, q1, q2 ] },
      If[ r === Infinity, {},
        Intersection[
          Select[ VertexList[ graph ], GraphDistance[ graph, q1, # ] == r & ],
          Select[ VertexList[ graph ], GraphDistance[ graph, q2, # ] == r & ] ]
      ]
    ], p1, p2 ]


(* ===================== FindInfraCommonPoint ===================== *)

(* Vertices common to every listed line: the intersection of the lines.
   Each input line is a bare vertex sequence or a wrapped InfraLine /
   InfraSegment / InfraPath / InfraRay -- unwrap via linePointSet (Tools.wl). *)

FindInfraCommonPoint[ graph_Graph, lines_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { vs = If[ Length[ lines ] == 0, {},
        Apply[ Intersection, linePointSet /@ lines ] ] },
    With[ { capped = infraCap[ vs, count ] },
      If[ capped === $Failed, $Failed, InfraPoint[ { # } ] & /@ capped ]
    ]
  ]


(* ===================== FindClosestInfraPoint ===================== *)

(* Metric argmin: vertices on line at minimum graph distance from point.
   Distinct from FindInfraPerpendicular -- that runs the synthetic Euclid I.12
   foot (midpoints of equidistant pairs); this is the closest-vertex test. *)

FindClosestInfraPoint[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  spreadFind[ InfraPoint, count,
    { line0, point0 } |-> MinimalBy[ line0, GraphDistance[ graph, point0, # ] & ], line, point ]


(* ===================== SelectInfraPoint ===================== *)

(* Chainable post-filter on a bundle of vertices treated as a finite metric
   space under the graph distance.  Pool selectors mirror FindInfraPoint;
   "Center" / "Periphery" use sub-bundle eccentricity, "Distance" enforces a
   mutual-distance clique on the n returned vertices. *)

Options[ SelectInfraPoint ] = { "From" -> All, "Distance" -> None, "MaxCliques" -> All };

SelectInfraPoint[ graph_Graph, vertices_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  InfraPoint[ { # } ] & /@ selectFromPointSpace[ graph, vertices, n,
    OptionValue[ "From" ], OptionValue[ "Distance" ], OptionValue[ "MaxCliques" ] ]

SelectInfraPoint[ graph_Graph, vertices_List, All, opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  SelectInfraPoint[ graph, vertices, UpTo[ Length[ vertices ] ], opts ]

SelectInfraPoint[ graph_Graph, vertices_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  With[ { result = SelectInfraPoint[ graph, vertices, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectInfraPoint[ graph_Graph, InfraPoint[ vs_List ],
                  countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectInfraPoint[ graph, vs, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraPoint[ First /@ result ] ] ]

SelectInfraPoint[ graph_Graph, list_List,
                  countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  SelectInfraPoint[ graph, First /@ list, countSpec, opts ]

(* Any set-like Infra* wrapper is a vertex bundle: select from its vertex set. *)
SelectInfraPoint[ graph_Graph,
                  bundle : ( InfraBall | InfraShell | InfraEllipticShell | InfraPlane | InfraSet | InfraObject | InfraCircle | InfraEllipse )[ _List ],
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
    With[ { anchorDists = Association[ # -> GraphDistance[ graph, # ] & /@ anchors ] },
      Flatten @ Position[ vertices,
        v_ /; AllTrue[ anchors, a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ] ],
        { 1 }, Heads -> False ]
    ]
  ]

pointPoolPositions[ _, vertices_List, InfraPoint[ reps_List ], _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ reps, { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, v_, _ ] /; MemberQ[ vertices, v ] :=
  { First @ FirstPosition[ vertices, v ] }

pointPoolPositions[ _, vertices_List, list_List, _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ list, { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, _, _ ] := Range @ Length @ vertices


(* ===================== InfraReachableQ ===================== *)
(* p1, p2 share a connected component: exists v in p1, w in p2 with v ~ w.
   Single multi-source BFS via VertexComponent -- visits only the components
   touched by p1's realisations, no full-graph component partition built. *)

InfraReachableQ[ graph_Graph, p1_, p2_ ] :=
  IntersectingQ[ VertexComponent[ graph, infraPointVertices @ p1 ], infraPointVertices @ p2 ]

infraPointVertices[ InfraPoint[ vs_List ] ] := vs
infraPointVertices[ list_List ]              := list
infraPointVertices[ v_ ]                     := { v }


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraPoint[ ] ] :=
  VertexList @ graph

dispatchConstruction[ graph_Graph, InfraPoint[ v_ ] ] /; MemberQ[ VertexList @ graph, v ] :=
  { v }

(* A predefined, already-resolved point: the unary/multi InfraPoint[{v, ...}]
   wrapper returned by FindInfraPoint et al.  Each underlying vertex is a
   realisation, so a scene symbol can be bound directly to a computed point
   (p == FindInfraPoint[graph, ...][[1]]) without unwrapping. *)
dispatchConstruction[ graph_Graph, InfraPoint[ vs_List ] ] /;
    vs =!= { } && SubsetQ[ VertexList @ graph, vs ] :=
  vs

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
