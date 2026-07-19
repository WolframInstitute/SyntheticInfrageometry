Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== FindInfraRadarBasis / InfraRadarBasisQ (deprecated) ===================== *)

(* The radar basis (resolving set) and the radar map now live in the Infrageometry
   paclet as FindResolvingSet / ResolvingSet / MetricDimension / RadarCoordinates.
   FindInfraRadarBasis / InfraRadarBasisQ are kept as deprecation aliases. *)

FindInfraRadarBasis[ args___ ] := FindResolvingSet[ args ]
InfraRadarBasisQ[ args___ ] := ResolvingSetQ[ args ]


(* ===================== RadarCoordinates ===================== *)

(* Distance vector of v wrt basis b: (d(v, b1), ..., d(v, bk)).
   An InfraPoint[{u1, ..., um}] entry in the basis contributes the aggregated
   distance Min | Mean | Max over its realisations (Min = the infra-observer's
   nearest-anchor reading, default).  The bulk form RadarCoordinates[g, b]
   returns an Association of all vertices' radar coordinates. *)

(* The crisp RadarCoordinates[g, b, v] / [g, b] now lives in Infrageometry.  Here
   we add the InfraObject overloads: an InfraPoint query point, and InfraPoint
   anchors in the basis aggregated by "InfraPointAggregation" (Min default). *)

Options[ RadarCoordinates ] = { "InfraPointAggregation" -> Min }

(* InfraPoint anchors in the basis -- more specific than the crisp Infrageometry
   pattern b_List, so this is tried first. *)
RadarCoordinates[ g_Graph, b : { ___, _InfraPoint, ___ }, v : Except[ _Rule | _RuleDelayed | _InfraPoint ], opts : OptionsPattern[] ] :=
  With[ { agg = OptionValue[ "InfraPointAggregation" ] },
    infraAnchorDistance[ g, v, #, agg ] & /@ b
  ]

RadarCoordinates[ g_Graph, b_List, InfraPoint[ { v_ } ], opts : OptionsPattern[] ] :=
  RadarCoordinates[ g, b, v, opts ]

RadarCoordinates[ g_Graph, b_List, InfraPoint[ vs_List ], opts : OptionsPattern[] ] /;
  Length[ vs ] > 1 && SubsetQ[ VertexList[ g ], vs ] :=
  RadarCoordinates[ g, b, #, opts ] & /@ vs

(* The crisp Infrageometry definitions load first, so Mathematica keeps them
   ahead of these InfraPoint overloads in DownValues order.  Reorder so the
   InfraPoint rules (which are the more specific cases) are tried first. *)
DownValues[ RadarCoordinates ] = SortBy[ DownValues[ RadarCoordinates ], FreeQ[ #, InfraPoint ] & ]


(* ===================== OrthogonalCoordinates ===================== *)

(* OrthogonalCoordinates[g, c, {a1, ..., an}, v] projects v onto each axis ai
   through the centre c by shortest-path distance and returns the tuple of
   Z-valued displacements with c at {0, ..., 0}.  Each axis is signed relative
   to the first vi in the (InfraPoint) centre order that lies on it.  When
   the projection is tied, "SelectCoordinate" chooses the reducer:
   "Centered" (default): 0 if 0 in the shifted tie list, else Round[Median[.]];
   Min/Max/Mean/Median/First/Last: linear reducers (commute with the shift);
   All: full shifted tie list;  any user function: applied to the shifted list. *)

Options[ OrthogonalCoordinates ] = { "SelectCoordinate" -> "Centered" };

OrthogonalCoordinates[ g_Graph, c_, axes_List, v_, opts : OptionsPattern[] ] /;
    MemberQ[ VertexList[ g ], v ] :=
  With[ {
      centerVs  = Replace[ c, { InfraPoint[ vs_List ] :> vs, x_ :> { x } } ],
      axisPaths = Replace[ #, InfraSegment[ reps_List ] :> First @ reps ] & /@ axes,
      sel       = OptionValue[ "SelectCoordinate" ]
    },
    Map[
      axis |-> selectCoordinate[ sel,
        axisLayerIndex[ g, axis, v ] -
          First @ axisLayerIndex[ g, axis, perAxisAnchor[ axis, centerVs ] ] ],
      axisPaths ]
  ]

OrthogonalCoordinates[ g_Graph, c_, axes_List, opts : OptionsPattern[] ] :=
  Association[ # -> OrthogonalCoordinates[ g, c, axes, #, opts ] & /@ VertexList[ g ] ]


(* ===================== FindInfraOrthogonalFrame ===================== *)

(* Returns a list of axes mutually perpendicular at the centre c, each wrapped
   as InfraSegment[{path}] (one realisation: the maximal metric line through c
   with c strictly interior).  Algorithm: build GeodesicSprayGraph[g, c]; enumerate
   candidate lines via antipodal DAG-vertex pairs; DFS the choice tree,
   filtering the DAG by perpendicularity at each step.

   Perpendicularity at c of axes A, B: every vertex w of B has c's axis-index
   on A among w's tied closest positions on A (and symmetrically).

   axisLength (required positional): half-axis depth spec.  All = any depth;
   n = exactly n (local axis filling the radius-n ball); UpTo[n] = at most n;
   {min, max} = explicit range.  Option "AxisCount" filters frame sizes:
   Automatic (saturated leaves), k (exactly k), UpTo[k] (k or saturation),
   All (every depth >= 1).  Method -> Automatic = "Exhaustive" ranked by
   frameSortKey; "Greedy" keeps DFS order. *)

Options[ FindInfraOrthogonalFrame ] = {
  Method             -> Automatic,
  "AxisCount"        -> Automatic,
  "BranchSampleSize" -> All,
  "SelectCoordinate" -> "Centered"
};

axisLengthPattern = All | _Integer | _UpTo | { _, _ };

FindInfraOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { result = findOrthogonalFrameCore[ g, c, axisLength, 1, { opts } ] },
    If[ result =!= { }, wrapFrame @ First @ result, $Failed ]
  ]

FindInfraOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  wrapFrame /@ findOrthogonalFrameCore[ g, c, axisLength, All, { opts } ]

FindInfraOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  wrapFrame /@ Take[ findOrthogonalFrameCore[ g, c, axisLength, n, { opts } ], UpTo[ n ] ]

FindInfraOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, n_Integer, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { result = findOrthogonalFrameCore[ g, c, axisLength, n, { opts } ] },
    If[ Length[ result ] >= n, wrapFrame /@ Take[ result, n ], $Failed ]
  ]

(* InfraPoint centre: singleton degenerates to the single-vertex centre;
   multi-vertex InfraPoint runs the search per source and merges. *)

FindInfraOrthogonalFrame[ g_Graph, InfraPoint[ { v_ } ], rest___ ] :=
  FindInfraOrthogonalFrame[ g, v, rest ]

FindInfraOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = findOrthogonalFrameCore[ g, ip, axisLength, 1, { opts } ] },
    If[ result =!= { }, wrapFrame @ First @ result, $Failed ]
  ]

FindInfraOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, All, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  wrapFrame /@ findOrthogonalFrameCore[ g, ip, axisLength, All, { opts } ]

FindInfraOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  wrapFrame /@ Take[ findOrthogonalFrameCore[ g, ip, axisLength, n, { opts } ], UpTo[ n ] ]

FindInfraOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, n_Integer, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = findOrthogonalFrameCore[ g, ip, axisLength, n, { opts } ] },
    If[ Length[ result ] >= n, wrapFrame /@ Take[ result, n ], $Failed ]
  ]


(* ===================== FindInfraSpanningAxes ===================== *)

(* No-center form: greedy mutually-separated longest geodesics across the
   whole graph.  Reuses orthogonalGreedy with Hausdorff-separation options. *)

Options[ FindInfraSpanningAxes ] = {
  "AxisDistance"  -> "MinEndpoint",
  "MinLength"     -> Automatic,
  "MinSeparation" -> Automatic,
  "AxisThickness" -> 0,
  "RandomPick"    -> False
};

FindInfraSpanningAxes[ g_Graph, All, opts : OptionsPattern[] ] :=
  With[ { distMatrix = GraphDistanceMatrix[ g ] },
    With[ { minLength = Replace[ OptionValue[ "MinLength" ], Automatic -> Max[ distMatrix ] ] },
      orthogonalGreedy[ g, findLongestPaths[ g, All, Max[ distMatrix ] - minLength ], { opts } ]
    ]
  ]

FindInfraSpanningAxes[ g_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindInfraSpanningAxes[ g, All, opts ], UpTo[ n ] ]

FindInfraSpanningAxes[ g_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindInfraSpanningAxes[ g, UpTo[ n ], opts ] },
    If[ Length[ result ] >= n, Take[ result, n ], $Failed ]
  ]


(* ===================== ResistanceCoordinates ===================== *)

(* Resistance-matching spectral embedding
       Phi(v) = (phi_i(v) / Sqrt[lambda_i])_{i: lambda_i > 0}
   satisfying ||Phi(u) - Phi(v)||^2 == R(u, v) (Klein-Randic).  Other
   "Rescaling" options: "None" (plain Laplacian eigenvectors),
   "Diffusion" -> t (diffusion-map embedding).  "Dimension" caps how many
   smallest non-zero modes are kept; "Origin" shifts the embedding so the
   chosen vertex (or InfraPoint centroid) lands at the origin. *)

(* The crisp embedding ResistanceCoordinates[g] / [g, v] (Options "Rescaling",
   "Dimension", "Origin") is relocated to the Infrageometry paclet.  The InfraPoint
   query overloads stay here. *)

ResistanceCoordinates[ g_Graph, InfraPoint[ { v_ } ], opts : OptionsPattern[] ] :=
  ResistanceCoordinates[ g, v, opts ]

ResistanceCoordinates[ g_Graph, InfraPoint[ vs_List ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { all = ResistanceCoordinates[ g, opts ] }, all /@ vs ]


(* ===================== Helpers: anchor distance ===================== *)

infraAnchorDistance[ g_, v_, InfraPoint[ vs_List ], agg_ ] :=
  agg[ GraphDistance[ g, v, # ] & /@ vs ]

infraAnchorDistance[ g_, v_, u_, _ ] :=
  GraphDistance[ g, v, u ]


(* ===================== Helpers: orthogonal coordinates ===================== *)

(* axisLayerIndex projects v onto an axis (a vertex sequence or a DAG of
   dependencies) and returns every 0-based layer tied at the minimum graph
   distance.  Callers reduce the list via "SelectCoordinate". *)

axisLayerIndex[ g_Graph, axis_List, v_ ] :=
  With[ { dists = GraphDistance[ g, v, # ] & /@ axis },
    Flatten @ Position[ dists, Min @ dists ] - 1
  ]

axisLayerIndex[ g_Graph, dag_Graph, v_ ] :=
  With[ { verts = VertexList[ dag ] },
    With[ { sources = Select[ verts, VertexInDegree[ dag, # ] == 0 & ] },
      With[ { depth = u |-> Min[ GraphDistance[ dag, #, u ] & /@ sources ] },
        With[ { layers = Table[ Select[ verts, depth[ # ] == k & ], { k, 0, Max[ depth /@ verts ] } ],
                dists  = GraphDistance[ g, v, # ] & /@ verts },
          With[ { proj = Pick[ verts, dists, Min @ dists ] },
            Flatten @ Table[ Position[ layers, u ][[ All, 1 ]] - 1, { u, proj } ]
          ]
        ]
      ]
    ]
  ]


selectCoordinate[ "Centered", shifted_List ] :=
  If[ MemberQ[ shifted, 0 ], 0, Round @ Median[ shifted ] ]
selectCoordinate[ All, ix_List ] := ix
selectCoordinate[ f_,   ix_List ] := f @ ix


perAxisAnchor[ axis_List, vs_List ] :=
  SelectFirst[ vs, MemberQ[ axis, # ] &, First @ vs ]

perAxisAnchor[ axis_Graph, vs_List ] :=
  SelectFirst[ vs, MemberQ[ VertexList @ axis, # ] &, First @ vs ]


(* ===================== Helpers: orthogonal-frame search ===================== *)

(* allHalfAxes: every directed c -> v path in dag. *)

allHalfAxes[ dag_Graph, c_ ] :=
  Catenate[ FindPath[ dag, c, #, Infinity, All ] & /@ VertexList[ dag ] ]


(* enumerateAxes: every candidate line through c with both half-axes of depth
   >= minLength.  Pairs half-axes with antipodal endpoints; dedup on the
   orientation-canonical vertex sequence. *)

enumerateAxes[ g_Graph, dag_Graph, c_, minLength_Integer ] :=
  With[ { dist = AssociationThread[ VertexList[ dag ], GraphDistance[ dag, c, # ] & /@ VertexList[ dag ] ],
          halvesByEnd = GroupBy[ allHalfAxes[ dag, c ], Last ] },
    With[ { vertsAtDepth = Select[ VertexList[ dag ], dist[ # ] >= minLength & ] },
      DeleteDuplicatesBy[
        Catenate @ Map[
          pair |-> Flatten[
            Outer[
              { hPos, hNeg } |-> Join[ Reverse @ hNeg, Rest @ hPos ],
              halvesByEnd[ pair[[ 1 ]] ], halvesByEnd[ pair[[ 2 ]] ], 1 ], 1 ],
          Select[ Subsets[ vertsAtDepth, { 2 } ],
            pair |-> GraphDistance[ g, pair[[ 1 ]], pair[[ 2 ]] ] === dist[ pair[[ 1 ]] ] + dist[ pair[[ 2 ]] ] ]
        ],
        First @ Sort[ { #, Reverse @ # } ] &
      ]
    ]
  ]


(* axisSortKey: length-first, then ascending endpoint-geodesic-multiplicity
   so straight axes (unique geodesic between endpoints) outrank L-shapes
   (many parallel geodesics) on grids, then lex-min for full determinism. *)

axisSortKey[ axisMult_ ][ axis_List ] :=
  { -Length[ axis ], axisMult[ axis ], Min[ axis, Reverse @ axis ] }


(* projectsToCenterQ: does w project to c on axis under tie-reducer sel?
   The same definition as the OrthogonalCoordinates coord being 0. *)

projectsToCenterQ[ g_Graph, axis_, c_, w_, sel_ ] :=
  selectCoordinate[ sel,
    axisLayerIndex[ g, axis, w ] - First @ axisLayerIndex[ g, axis, c ] ] === 0


restrictDagToCenter[ g_Graph, dag_Graph, axis_List, c_, sel_ ] :=
  Subgraph[ dag, Select[ VertexList[ dag ], projectsToCenterQ[ g, axis, c, #, sel ] & ] ]


canonicalFrame[ axes_List ] :=
  Sort[ First @ Sort[ { #, Reverse @ # } ] & /@ axes ]


frameSortKey[ axisMult_ ][ frame_List ] :=
  { -Length[ frame ], -Total[ Length /@ frame ],
    Total[ axisMult /@ frame ], canonicalFrame[ frame ] }


(* axisMultiplicityFn[g]: closure axis |-> GeodesicMultiplicity[g, First, Last]
   precomputed via GeodesicMultiplicityMatrix so per-axis lookup is O(1). *)

axisMultiplicityFn[ g_Graph ] :=
  With[ { mMat   = Last @ GeodesicMultiplicityMatrix[ g ],
          posMap = AssociationThread[ VertexList[ g ] -> Range @ VertexCount[ g ] ] },
    axis |-> mMat[[ posMap[ First @ axis ], posMap[ Last @ axis ] ]]
  ]


(* recordFrameQ / recurseDFSQ: when to record / when to keep descending
   for each "AxisCount" spec. *)

recordFrameQ[ Automatic ][ len_, vAxes_ ] := vAxes === { } && len > 0
recordFrameQ[ All       ][ len_, _ ]       := len > 0
recordFrameQ[ n_Integer ][ len_, _ ]       := len === n
recordFrameQ[ UpTo[ n_ ] ][ len_, vAxes_ ] := len === n || ( vAxes === { } && len > 0 )

recurseDFSQ[ Automatic ][ _, vAxes_ ]     := vAxes =!= { }
recurseDFSQ[ All       ][ _, vAxes_ ]     := vAxes =!= { }
recurseDFSQ[ n_Integer ][ len_, vAxes_ ]  := len < n && vAxes =!= { }
recurseDFSQ[ UpTo[ n_ ] ][ len_, vAxes_ ] := len < n && vAxes =!= { }


orthogonalFrameDFS[ g_Graph, c_, fullDag_Graph, axisCountSpec_, minLength_, sampleSize_, maxFrames_, sel_, axisMult_, perpQ_ ] :=
  Module[ { frames = { }, canonForms = { }, dfs },
    dfs[ dag_, currentAxes_ ] :=
      Module[ { len, axisCands, validAxes, sortedAxes, sampledAxes, canon },
        len = Length[ currentAxes ];
        axisCands = enumerateAxes[ g, dag, c, minLength ];
        validAxes = Select[ axisCands, perpQ[ currentAxes, # ] & ];
        If[ recordFrameQ[ axisCountSpec ][ len, validAxes ],
          canon = canonicalFrame[ currentAxes ];
          If[ ! MemberQ[ canonForms, canon ],
            AppendTo[ canonForms, canon ];
            AppendTo[ frames, currentAxes ];
            If[ Length[ frames ] >= maxFrames, Throw[ Null ] ]
          ]
        ];
        If[ recurseDFSQ[ axisCountSpec ][ len, validAxes ],
          sortedAxes = SortBy[ validAxes, axisSortKey[ axisMult ] ];
          sampledAxes = If[ sampleSize === All || Length[ sortedAxes ] <= sampleSize,
            sortedAxes,
            RandomSample[ sortedAxes, sampleSize ] ];
          Scan[
            axis |-> dfs[ restrictDagToCenter[ g, dag, axis, c, sel ], Append[ currentAxes, axis ] ],
            sampledAxes ]
        ]
      ];
    Catch[ dfs[ fullDag, { } ] ];
    frames
  ]


parseAxisLengthSpec[ All ]            := { 1, Infinity }
parseAxisLengthSpec[ n_Integer ]      := { n, n }
parseAxisLengthSpec[ UpTo[ n_ ] ]     := { 1, n }
parseAxisLengthSpec[ { min_, max_ } ] := { min, max }


resolveSearchMethod[ opts_List ] :=
  Replace[ Method /. opts /. Method -> Automatic, Automatic -> "Exhaustive" ]


(* Default perpendicularity oracle: for every previously chosen axis, every one
   of its vertices projects to the centre on the candidate.  Matches the
   index-based test that drove the frame search before Method -> "Predicate". *)

inlineFramePerpQ[ g_Graph, c_, sel_ ][ currentAxes_, cand_ ] :=
  AllTrue[ currentAxes,
    prev |-> AllTrue[ prev, w |-> projectsToCenterQ[ g, cand, c, w, sel ] ] ]

(* Predicate-driven perpendicularity oracle: route each axis pair through
   InfraPerpendicularQ with caller-supplied sub-options. *)

predicateFramePerpQ[ g_Graph, predOpts_List ][ currentAxes_, cand_ ] :=
  AllTrue[ currentAxes, prev |-> InfraPerpendicularQ[ g, prev, cand, Sequence @@ predOpts ] ]


resolveFramePerpQ[ g_Graph, c_, sel_, methodSpec_ ] :=
  If[ methodName @ methodSpec === "Predicate",
    predicateFramePerpQ[ g, predicateSubOpts @ propertiesSubOpts @ methodSpec ],
    inlineFramePerpQ[ g, c, sel ]
  ]


(* Translate {"Predicate", "Test" -> spec, "Radius" -> r, "Tolerance" -> t}
   sub-options into InfraPerpendicularQ's option list.  "Test" -> Automatic
   defaults to InfraPerpendicularQ's own default (Method -> "Projection"). *)

predicateSubOpts[ subOpts_List ] :=
  With[ { testVal = "Test" /. subOpts /. { "Test" -> Automatic } },
    Join[
      If[ testVal === Automatic, { }, { Method -> testVal } ],
      Cases[ subOpts, ( "Radius" | "Tolerance" | "Equality" ) -> _ ]
    ]
  ]


findOrthogonalFrameCore[ g_Graph, c_, axisLength_, count_, opts_List ] /; MemberQ[ VertexList[ g ], c ] :=
  Module[ { minLength, maxDepth, localG },
    { minLength, maxDepth } = parseAxisLengthSpec[ axisLength ];
    (* Localize: every distance the search needs lies in B(c, 2 maxDepth). *)
    localG = If[ maxDepth === Infinity, g, NeighborhoodGraph[ g, c, 2 maxDepth ] ];
    With[ { dag = GeodesicSprayGraph[ localG, c, "AxisLength" -> Replace[ maxDepth, Infinity -> All ] ],
            axisCountSpec = "AxisCount" /. opts /. "AxisCount" -> Automatic,
            methodSpec = resolveSearchMethod[ opts ],
            sel = "SelectCoordinate" /. opts /. "SelectCoordinate" -> "Centered",
            axisMult = axisMultiplicityFn[ localG ] },
      With[ { method = methodName @ methodSpec,
              perpQ  = resolveFramePerpQ[ localG, c, sel, methodSpec ] },
        With[ { sampleSize = If[ method === "Greedy", All,
                    "BranchSampleSize" /. opts /. "BranchSampleSize" -> All ],
                maxFrames  = If[ method === "Greedy" && IntegerQ @ count, count, Infinity ] },
          With[ { frames = orthogonalFrameDFS[ localG, c, dag, axisCountSpec, minLength, sampleSize, maxFrames, sel, axisMult, perpQ ] },
            If[ method === "Greedy", frames, SortBy[ frames, frameSortKey[ axisMult ] ] ]
          ]
        ]
      ]
    ]
  ]

findOrthogonalFrameCore[ g_Graph, InfraPoint[ vs_List ], axisLength_, count_, opts_List ] :=
  With[ { method = methodName @ resolveSearchMethod[ opts ],
          axisMult = axisMultiplicityFn[ g ] },
    With[ { perSource = Map[ findOrthogonalFrameCore[ g, #, axisLength, All, opts ] &, vs ] },
      With[ { allFrames = DeleteDuplicatesBy[ Catenate @ perSource, canonicalFrame ] },
        With[ { sortedFrames = If[ method === "Greedy", allFrames, SortBy[ allFrames, frameSortKey[ axisMult ] ] ],
                maxFrames    = If[ count === All, Infinity, count ] },
          Take[ sortedFrames, UpTo[ maxFrames ] ]
        ]
      ]
    ]
  ]


wrapFrame[ frame_List ] := InfraSegment[ { # } ] & /@ frame


(* ===================== Helpers: longest paths / spanning axes ===================== *)

findLongestPaths[ g_Graph, n_, epsilon_ : 0 ] :=
  With[ { distMatrix = GraphDistanceMatrix[ g ], vertices = VertexList[ g ] },
    With[ { maxDist = Max[ distMatrix ] },
      With[ { pairs = Select[
              DeleteDuplicatesBy[ Position[ distMatrix, _?( # >= maxDist - epsilon & ) ], Sort ],
              #[[ 1 ]] =!= #[[ 2 ]] & ] },
        With[ { numPairs = Length[ pairs ] },
          If[ numPairs == 0, { },
            With[ { counts = If[ n === All,
                  ConstantArray[ All, numPairs ],
                  RandomSample @ Table[ Quotient[ n, numPairs ] + Boole[ i <= Mod[ n, numPairs ] ], { i, numPairs } ] ] },
              Flatten[
                Cases[
                  Transpose[ { pairs, counts } ],
                  { { i_, j_ }, cnt_ /; cnt =!= 0 } :>
                    FindPath[ g, vertices[[ i ]], vertices[[ j ]], { distMatrix[[ i, j ]] }, cnt ] ],
                1 ]
            ]
          ]
        ]
      ]
    ]
  ]


orthogonalGreedy[ g_Graph, paths_List, opts_List ] :=
  Module[ { axes, candidates, next, previousIndices, previousEndpoints, separation, closeAxes, scores,
            vertices = VertexList[ g ],
            distMatrix = GraphDistanceMatrix[ g ],
            distanceFunction = "AxisDistance" /. opts /. "AxisDistance" -> "MinEndpoint",
            thickness = "AxisThickness" /. opts /. "AxisThickness" -> 0,
            pick = If[ ! TrueQ[ "RandomPick" /. opts /. "RandomPick" -> False ], First, RandomChoice ] },
    If[ paths === { }, Return[ { } ] ];
    With[ { vertexIndex = AssociationThread[ vertices, Range @ Length @ vertices ],
            minSeparation = Replace[ "MinSeparation" /. opts /. "MinSeparation" -> Automatic,
              Automatic -> ( Length[ First[ paths ] ] - 1 ) / 2 ] },
      axes = { pick[ paths ] };
      previousIndices = Lookup[ vertexIndex, axes[[ 1 ]] ];
      previousEndpoints = { vertexIndex[ axes[[ 1, 1 ]] ], vertexIndex[ axes[[ 1, -1 ]] ] };
      candidates = Complement[ paths, axes ];
      While[ candidates =!= { },
        scores = Switch[ distanceFunction,
          "MinEndpoint",
            ( Min[
                distMatrix[[ vertexIndex[ #[[ 1 ]] ], previousEndpoints ]],
                distMatrix[[ vertexIndex[ #[[ -1 ]] ], previousEndpoints ]] ] & ) /@ candidates,
          "Hausdorff",
            ( p |-> HausdorffDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates,
          "Separation",
            ( p |-> MinimalSeparationDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates,
          _, Return[ axes ]
        ];
        separation = Max[ scores ];
        If[ separation < minSeparation, Break[ ] ];
        next = pick[ candidates[[ Flatten @ Position[ scores, separation ] ]] ];
        closeAxes = If[ thickness == 0, { next },
          Select[ candidates,
            HausdorffDistance[ distMatrix, Lookup[ vertexIndex, # ], Lookup[ vertexIndex, next ] ] <= thickness & ] ];
        axes = Join[ axes, closeAxes ];
        previousIndices = Union[ previousIndices, Flatten[ Lookup[ vertexIndex, # ] & /@ closeAxes ] ];
        previousEndpoints = Union[ previousEndpoints,
          Flatten[ { vertexIndex[ #[[ 1 ]] ], vertexIndex[ #[[ -1 ]] ] } & /@ closeAxes ] ];
        candidates = Complement[ candidates, closeAxes ]
      ];
      axes
    ]
  ]


(* resistanceEmbeddingMatrix relocated to the Infrageometry paclet. *)
