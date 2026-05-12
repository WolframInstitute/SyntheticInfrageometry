Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindRadarBasis ===================== *)

(* A radar basis (resolving set) is a vertex set B such that the distance
   vector v |-> (d(v, b))_{b in B} is injective.  Enumerated by ascending
   size; m restricts the candidate sizes (All, integer max, {min, max},
   {exact}). *)

FindRadarBasis[ g_, n_ : 1, m_ : All ] :=
  Module[ { v = VertexList[ g ], dm = GraphDistanceMatrix[ g ], vc = VertexCount[ g ], found = { }, mask, last },
    Map[ v[[ # ]] &,
      Catch[ Scan[
        k |-> (
          mask = 2^k - 1;
          last = BitShiftLeft[ 2^k - 1, vc - k ];
          While[ mask <= last,
            With[ { s = Pick[ Range[ vc ], IntegerDigits[ mask, 2, vc ], 1 ] },
              If[ DuplicateFreeQ[ dm[[ All, s ]] ],
                AppendTo[ found, s ];
                If[ Length @ found >= n, Throw[ found ] ]
              ]
            ];
            (* Gosper's hack: next k-subset of {1, ..., vc} in lex-on-bitmasks
               order.  c isolates the lowest set bit; r = mask + c carries
               that bit's run leftward; the BitOr restores the displaced
               lower bits at the smallest positions. *)
            mask = With[ { c = BitAnd[ mask, -mask ] }, { r = mask + c },
              BitOr[ r, Quotient[ BitXor[ r, mask ], 4 c ] ] ]
          ]
        ),
        Replace[ m, {
          All :> Range[ vc ],
          _Integer :> Range[ m ],
          { min_, max_ } :> Range[ min, max ],
          { num_ } :> { num }
        } ]
      ]; Throw[ found ] ]
    ]
  ]


(* ===================== RadarBasisQ ===================== *)

(* b resolves g iff the pointwise distance map v |-> (d(v, b1), ..., d(v, bk))
   is injective over V(g). *)

Options[ RadarBasisQ ] = { "InfraPointAggregation" -> Min }

RadarBasisQ[ g_Graph, b_List, opts : OptionsPattern[] ] :=
  With[ { agg = OptionValue[ "InfraPointAggregation" ] },
    DuplicateFreeQ[ Table[ infraAnchorDistance[ g, v, #, agg ] & /@ b, { v, VertexList[ g ] } ] ]
  ]


(* ===================== RadarCoordinates ===================== *)

(* Distance vector of v wrt basis b: (d(v, b1), ..., d(v, bk)).
   An InfraPoint[{u1, ..., um}] entry in the basis contributes the aggregated
   distance Min | Mean | Max over its realisations (Min = the infra-observer's
   nearest-anchor reading, default).  The bulk form RadarCoordinates[g, b]
   returns an Association of all vertices' radar coordinates. *)

Options[ RadarCoordinates ] = { "InfraPointAggregation" -> Min }

RadarCoordinates[ g_Graph, b_List, v : Except[ _Rule | _RuleDelayed | _InfraPoint ], opts : OptionsPattern[] ] :=
  With[ { agg = OptionValue[ "InfraPointAggregation" ] },
    infraAnchorDistance[ g, v, #, agg ] & /@ b
  ]

RadarCoordinates[ g_Graph, b_List, InfraPoint[ { v_ } ], opts : OptionsPattern[] ] :=
  RadarCoordinates[ g, b, v, opts ]

RadarCoordinates[ g_Graph, b_List, InfraPoint[ vs_List ], opts : OptionsPattern[] ] /;
  SubsetQ[ VertexList[ g ], vs ] :=
  RadarCoordinates[ g, b, #, opts ] & /@ vs

RadarCoordinates[ g_Graph, b_List, opts : OptionsPattern[] ] :=
  Association[ # -> RadarCoordinates[ g, b, #, opts ] & /@ VertexList[ g ] ]


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
    orthogonalCoordsCore[ g, axisPaths, v, perAxisAnchor[ #, centerVs ] & /@ axisPaths, sel ]
  ]

OrthogonalCoordinates[ g_Graph, c_, axes_List, opts : OptionsPattern[] ] :=
  Association[ # -> OrthogonalCoordinates[ g, c, axes, #, opts ] & /@ VertexList[ g ] ]


(* ===================== FindOrthogonalFrame ===================== *)

(* Returns a list of axes mutually perpendicular at the centre c, each wrapped
   as InfraSegment[{path}] (one realisation: the maximal metric line through c
   with c strictly interior).  Algorithm: build GeodesicGraph[g, c]; enumerate
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

Options[ FindOrthogonalFrame ] = {
  Method             -> Automatic,
  "AxisCount"        -> Automatic,
  "BranchSampleSize" -> All,
  "SelectCoordinate" -> "Centered"
};

axisLengthPattern = All | _Integer | _UpTo | { _, _ };

FindOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { result = findOrthogonalFrameCore[ g, c, axisLength, 1, { opts } ] },
    If[ result =!= { }, wrapFrame @ First @ result, $Failed ]
  ]

FindOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  wrapFrame /@ findOrthogonalFrameCore[ g, c, axisLength, All, { opts } ]

FindOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  wrapFrame /@ Take[ findOrthogonalFrameCore[ g, c, axisLength, n, { opts } ], UpTo[ n ] ]

FindOrthogonalFrame[ g_Graph, c_, axisLength : axisLengthPattern, n_Integer, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { result = findOrthogonalFrameCore[ g, c, axisLength, n, { opts } ] },
    If[ Length[ result ] >= n, wrapFrame /@ Take[ result, n ], $Failed ]
  ]

(* InfraPoint centre: singleton degenerates to the single-vertex centre;
   multi-vertex InfraPoint runs the search per source and merges. *)

FindOrthogonalFrame[ g_Graph, InfraPoint[ { v_ } ], rest___ ] :=
  FindOrthogonalFrame[ g, v, rest ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = findOrthogonalFrameCore[ g, ip, axisLength, 1, { opts } ] },
    If[ result =!= { }, wrapFrame @ First @ result, $Failed ]
  ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, All, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  wrapFrame /@ findOrthogonalFrameCore[ g, ip, axisLength, All, { opts } ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  wrapFrame /@ Take[ findOrthogonalFrameCore[ g, ip, axisLength, n, { opts } ], UpTo[ n ] ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], axisLength : axisLengthPattern, n_Integer, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = findOrthogonalFrameCore[ g, ip, axisLength, n, { opts } ] },
    If[ Length[ result ] >= n, wrapFrame /@ Take[ result, n ], $Failed ]
  ]


(* ===================== FindSpanningAxes ===================== *)

(* No-center form: greedy mutually-separated longest geodesics across the
   whole graph.  Reuses orthogonalGreedy with Hausdorff-separation options. *)

Options[ FindSpanningAxes ] = {
  "AxisDistance"  -> "MinEndpoint",
  "MinLength"     -> Automatic,
  "MinSeparation" -> Automatic,
  "AxisThickness" -> 0,
  "RandomPick"    -> False
};

FindSpanningAxes[ g_Graph, All, opts : OptionsPattern[] ] :=
  With[ { distMatrix = GraphDistanceMatrix[ g ] },
    With[ { minLength = Replace[ OptionValue[ "MinLength" ], Automatic -> Max[ distMatrix ] ] },
      orthogonalGreedy[ g, findLongestPaths[ g, All, Max[ distMatrix ] - minLength ], { opts } ]
    ]
  ]

FindSpanningAxes[ g_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindSpanningAxes[ g, All, opts ], UpTo[ n ] ]

FindSpanningAxes[ g_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindSpanningAxes[ g, UpTo[ n ], opts ] },
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

Options[ ResistanceCoordinates ] = {
  "Rescaling" -> "ResistanceMatching",
  "Dimension" -> Automatic,
  "Origin" -> None
};

ResistanceCoordinates[ g_Graph, opts : OptionsPattern[] ] :=
  With[ { mat = resistanceEmbeddingMatrix[ g, OptionValue[ "Rescaling" ], OptionValue[ "Dimension" ] ],
          origin = OptionValue[ "Origin" ],
          idx = AssociationThread[ VertexList[ g ], Range @ VertexCount[ g ] ] },
    With[ { originVec = Switch[ origin,
              None,                  ConstantArray[ 0., Length @ First @ mat ],
              InfraPoint[ { _ } ],   mat[[ idx[ origin[[ 1, 1 ]] ] ]],
              InfraPoint[ _List ],   Mean[ mat[[ idx /@ origin[[ 1 ]] ]] ],
              _,                     mat[[ idx[ origin ] ]]
            ] },
      AssociationThread[ VertexList[ g ], # - originVec & /@ mat ]
    ]
  ]

ResistanceCoordinates[ g_Graph, v_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], v ] :=
  ResistanceCoordinates[ g, opts ][ v ]

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


orthogonalCoordsCore[ g_Graph, axes_List, v_, anchors_List, sel_ ] :=
  MapThread[
    { axis, anchor } |-> selectCoordinate[ sel,
      axisLayerIndex[ g, axis, v ] - First @ axisLayerIndex[ g, axis, anchor ] ],
    { axes, anchors }
  ]


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


axisSortKey[ axis_List ] :=
  { -Length[ axis ], Min[ axis, Reverse @ axis ] }


(* projectsToCenterQ: does w project to c on axis under tie-reducer sel?
   The same definition as the OrthogonalCoordinates coord being 0. *)

projectsToCenterQ[ g_Graph, axis_, c_, w_, sel_ ] :=
  selectCoordinate[ sel,
    axisLayerIndex[ g, axis, w ] - First @ axisLayerIndex[ g, axis, c ] ] === 0


restrictDagToCenter[ g_Graph, dag_Graph, axis_List, c_, sel_ ] :=
  Subgraph[ dag, Select[ VertexList[ dag ], projectsToCenterQ[ g, axis, c, #, sel ] & ] ]


canonicalFrame[ axes_List ] :=
  Sort[ First @ Sort[ { #, Reverse @ # } ] & /@ axes ]


frameSortKey[ frame_List ] :=
  { -Length[ frame ], -Total[ Length /@ frame ], canonicalFrame[ frame ] }


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


orthogonalFrameDFS[ g_Graph, c_, fullDag_Graph, axisCountSpec_, minLength_, sampleSize_, maxFrames_, sel_ ] :=
  Module[ { frames = { }, canonForms = { }, dfs },
    dfs[ dag_, currentAxes_ ] :=
      Module[ { len, axisCands, validAxes, sortedAxes, sampledAxes, canon },
        len = Length[ currentAxes ];
        axisCands = enumerateAxes[ g, dag, c, minLength ];
        validAxes = Select[ axisCands,
          cand |-> AllTrue[ currentAxes,
            prev |-> AllTrue[ prev, w |-> projectsToCenterQ[ g, cand, c, w, sel ] ] ] ];
        If[ recordFrameQ[ axisCountSpec ][ len, validAxes ],
          canon = canonicalFrame[ currentAxes ];
          If[ ! MemberQ[ canonForms, canon ],
            AppendTo[ canonForms, canon ];
            AppendTo[ frames, currentAxes ];
            If[ Length[ frames ] >= maxFrames, Throw[ Null ] ]
          ]
        ];
        If[ recurseDFSQ[ axisCountSpec ][ len, validAxes ],
          sortedAxes = SortBy[ validAxes, axisSortKey ];
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


findOrthogonalFrameCore[ g_Graph, c_, axisLength_, count_, opts_List ] /; MemberQ[ VertexList[ g ], c ] :=
  Module[ { minLength, maxDepth },
    { minLength, maxDepth } = parseAxisLengthSpec[ axisLength ];
    With[ { dag = GeodesicGraph[ g, c, "AxisLength" -> Replace[ maxDepth, Infinity -> All ] ],
            axisCountSpec = "AxisCount" /. opts /. "AxisCount" -> Automatic,
            method = resolveSearchMethod[ opts ],
            sel = "SelectCoordinate" /. opts /. "SelectCoordinate" -> "Centered" },
      With[ { sampleSize = If[ method === "Greedy", All,
                  "BranchSampleSize" /. opts /. "BranchSampleSize" -> All ],
              maxFrames  = If[ method === "Greedy" && IntegerQ @ count, count, Infinity ] },
        With[ { frames = orthogonalFrameDFS[ g, c, dag, axisCountSpec, minLength, sampleSize, maxFrames, sel ] },
          If[ method === "Exhaustive", SortBy[ frames, frameSortKey ], frames ]
        ]
      ]
    ]
  ]

findOrthogonalFrameCore[ g_Graph, InfraPoint[ vs_List ], axisLength_, count_, opts_List ] :=
  With[ { method = resolveSearchMethod[ opts ] },
    With[ { perSource = Map[ findOrthogonalFrameCore[ g, #, axisLength, All, opts ] &, vs ] },
      With[ { allFrames = DeleteDuplicatesBy[ Catenate @ perSource, canonicalFrame ] },
        With[ { sortedFrames = If[ method === "Exhaustive", SortBy[ allFrames, frameSortKey ], allFrames ],
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


(* ===================== Helpers: resistance embedding ===================== *)

resistanceEmbeddingMatrix[ g_Graph, rescaling_, dimSpec_ ] :=
  Module[ { vals, vecs, ord },
    { vals, vecs } = Eigensystem[ N @ Normal @ KirchhoffMatrix[ g ] ];
    ord = Ordering[ vals ];
    { vals, vecs } = { vals[[ ord ]], vecs[[ ord ]] };
    With[ { tol = 10^-10 Max[ Abs @ vals, 1 ] },
      With[ { keep = Select[ Range @ Length @ vals, vals[[ # ]] > tol & ] },
        With[ { dim = Replace[ dimSpec, {
                Automatic | All :> Length[ keep ],
                UpTo[ k_Integer ] :> Min[ k, Length[ keep ] ],
                k_Integer :> Min[ k, Length[ keep ] ] } ] },
          With[ { idx = Take[ keep, dim ] },
            With[ { weights = Replace[ rescaling, {
                    "ResistanceMatching" :> 1 / Sqrt[ vals[[ idx ]] ],
                    "None" :> ConstantArray[ 1, Length[ idx ] ],
                    ( "Diffusion" -> t_ ) :> Exp[ -t vals[[ idx ]] ] } ] },
              Transpose[ weights vecs[[ idx ]] ]
            ]
          ]
        ]
      ]
    ]
  ]
