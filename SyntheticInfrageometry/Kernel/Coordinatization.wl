Package["WolframInstitute`SyntheticInfrageometry`"]


(* A spatial radar basis (resolving set) is a vertex set B such that the
   distance vector v |-> (d(v, b))_{b in B} is injective.  FindRadarBasis
   enumerates such bases by ascending size; m restricts the candidate
   sizes (All, integer max, {min, max}, or {exact}). *)

FindRadarBasis[ g_, n_ : 1, m_ : All ] :=
  Module[ { v = VertexList[ g ], dm = GraphDistanceMatrix[ g ], vc = VertexCount[ g ], found = {}, mask, last },
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
            (* Gosper's hack: given a bitmask with k bits set, compute the
               next k-subset of {1, ..., vc} in lexicographic-on-bitmasks
               order.  c isolates the lowest set bit; r = mask + c carries
               that bit's run leftward; the final BitOr restores the
               displaced lower bits at the smallest available positions. *)
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

(* RadarBasisQ tests whether a vertex list b resolves the graph: the
   pointwise distance map v |-> (d(v, b1), ..., d(v, bk)) is injective. *)

Options[ RadarBasisQ ] = { "InfraPointAggregation" -> Min }

RadarBasisQ[ g_Graph, b_List, opts : OptionsPattern[] ] :=
  With[ { agg = OptionValue[ "InfraPointAggregation" ] },
    DuplicateFreeQ[ Table[ infraAnchorDistance[ g, v, #, agg ] & /@ b, { v, VertexList[ g ] } ] ]
  ]


(* RadarCoordinates of v with respect to b is its distance vector
   (d(v, b1), ..., d(v, bk)).  The two-argument form RadarCoordinates[g, b]
   returns the Association of all vertices' radar coordinates - which is
   itself a function, so RadarCoordinates[g, b][v] is the natural operator
   form.

   InfraPoint anchors: an InfraPoint[{u1, ..., um}] entry in the basis
   contributes the aggregated distance Min | Mean | Max over its
   realisations (Min by default, the infra-observer's nearest-anchor
   reading).  An InfraPoint[...] query point degenerates to its single
   vertex when |vs| = 1 and otherwise returns the list of per-realisation
   coordinate vectors.  Aggregation is controlled by the
   "InfraPointAggregation" option. *)

Options[ RadarCoordinates ] = { "InfraPointAggregation" -> Min }

infraAnchorDistance[ g_, v_, InfraPoint[ vs_List ], agg_ ] :=
  agg[ GraphDistance[ g, v, # ] & /@ vs ]

infraAnchorDistance[ g_, v_, u_, _ ] :=
  GraphDistance[ g, v, u ]

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


(* ===================== Laminar layers ===================== *)

(* axisLayerIndex projects v onto an axis (a vertex sequence or a DAG of
   dependencies) and returns the *list* of all 0-based positions/layers
   tied at the minimum graph distance.  The DAG form computes layers as
   shortest-path depth from the DAG's sources, then maps each tied
   projection vertex to its layer.  Callers (OrthogonalCoordinates,
   projectsToOriginQ) reduce this list to a scalar via the
   "SelectCoordinate" option. *)

axisLayerIndex[ g_Graph, axis_List, v_ ] :=
  With[ { dists = GraphDistance[ g, v, # ] & /@ axis },
    Flatten @ Position[ dists, Min @ dists ] - 1
  ]

axisLayerIndex[ g_Graph, dag_Graph, v_ ] :=
  Module[ { verts = VertexList[ dag ], sources, depth, layers, dists, proj },
    sources = Select[ verts, VertexInDegree[ dag, # ] == 0 & ];
    depth   = u |-> Min[ GraphDistance[ dag, #, u ] & /@ sources ];
    layers  = Table[ Select[ verts, depth[ # ] == k & ], { k, 0, Max[ depth /@ verts ] } ];
    dists   = GraphDistance[ g, v, # ] & /@ verts;
    proj    = Pick[ verts, dists, Min @ dists ];
    Flatten @ Table[ Position[ layers, u ][[ All, 1 ]] - 1, { u, proj } ]
  ]


(* ===================== OrthogonalCoordinates ===================== *)

(* OrthogonalCoordinates[g, c, {a1, ..., an}, v] projects v onto each axis
   ai through the centre c by shortest-path distance and returns the tuple
   of Z-valued displacements with c at {0, ..., 0}.  The centre c is a
   vertex or InfraPoint[{v1, ..., vk}] - in the latter case each axis is
   signed relative to the first vi (in InfraPoint order) that lies on it.
   Each axis ai is an InfraSegment (the first realisation is used) or a
   bare vertex sequence.  The frame is supplied by the caller;
   FindOrthogonalFrame is the standalone search.

   When the projection is multi-valued (ties), the "SelectCoordinate"
   option chooses what to return: a function applied to the tied list
   (e.g. Min (default), Max, First, Last, Mean, Median, or any
   user-supplied List -> ?Number), or All to keep the full tied list as
   the per-axis coordinate.  The anchor side is always reduced via First,
   so anchored coordinates broadcast cleanly when the per-axis value is
   itself a list. *)

selectCoordinate[ All, ix_List ] := ix
selectCoordinate[ f_,   ix_List ] := f @ ix

orthogonalCoordsCore[ g_Graph, axes_List, v_, anchors_List, sel_ ] :=
  With[ { vIdx = selectCoordinate[ sel, axisLayerIndex[ g, #, v ] ] & /@ axes },
    vIdx - MapThread[ First @ axisLayerIndex[ g, #1, #2 ] &, { axes, anchors } ]
  ]

perAxisAnchor[ axis_List, vs_List ] :=
  SelectFirst[ vs, MemberQ[ axis, # ] &, First @ vs ]

perAxisAnchor[ axis_Graph, vs_List ] :=
  SelectFirst[ vs, MemberQ[ VertexList @ axis, # ] &, First @ vs ]


Options[ OrthogonalCoordinates ] = { "SelectCoordinate" -> Min };

(* Single vertex: signed displacement of v in the frame {a1, ..., an} centred at c *)
OrthogonalCoordinates[ g_Graph, c_, axes_List, v_, opts : OptionsPattern[] ] /;
    MemberQ[ VertexList[ g ], v ] :=
  With[ {
      centerVs  = Replace[ c, { InfraPoint[ vs_List ] :> vs, x_ :> { x } } ],
      axisPaths = Replace[ #, InfraSegment[ reps_List ] :> First @ reps ] & /@ axes,
      sel       = OptionValue[ "SelectCoordinate" ]
    },
    orthogonalCoordsCore[ g, axisPaths, v, perAxisAnchor[ #, centerVs ] & /@ axisPaths, sel ]
  ]

(* Bulk: Association of all vertices' signed coordinates *)
OrthogonalCoordinates[ g_Graph, c_, axes_List, opts : OptionsPattern[] ] :=
  Association[ # -> OrthogonalCoordinates[ g, c, axes, #, opts ] & /@ VertexList[ g ] ]


(* ===================== Orthogonal axes ===================== *)

(* FindOrthogonalAxes selects a maximal mutually-separated set of longest
   geodesics (or longest geodesics through a chosen center).
   "Orthogonality" is operationalised as a Hausdorff / endpoint
   separation between axes; greedy selection.  When constrained to pass
   through c as an interior point, each axis fixes a sign convention for
   the corresponding signed OrthogonalCoordinates layer.  An InfraPoint
   center accepts a vertex set; each returned axis must contain at least
   one of those vertices as an interior point. *)

findLongestPaths[ g_Graph, n_, epsilon_ : 0 ] :=
  Module[ { vertices, distMatrix, maxDist, pairs, numPairs, counts },
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    maxDist = Max[ distMatrix ];
    pairs = DeleteDuplicatesBy[ Position[ distMatrix, _?( # >= maxDist - epsilon & ) ], Sort ];
    pairs = Select[ pairs, #[[ 1 ]] =!= #[[ 2 ]] & ];
    numPairs = Length[ pairs ];
    If[ numPairs == 0, Return[ {} ] ];
    counts = If[ n === All,
      ConstantArray[ All, numPairs ],
      RandomSample @ Table[ Quotient[ n, numPairs ] + Boole[ i <= Mod[ n, numPairs ] ], { i, numPairs } ]
    ];
    Flatten[
      Cases[
        Transpose[ { pairs, counts } ],
        { { i_, j_ }, cnt_ /; cnt =!= 0 } :> FindPath[ g, vertices[[ i ]], vertices[[ j ]], { distMatrix[[ i, j ]] }, cnt ]
      ],
      1
    ]
  ]

orthogonalGreedy[ g_Graph, paths_List, opts_List ] :=
  Module[ { distMatrix, vertices, vertexIndex, minSeparation, thickness, distanceFunction, pick,
            pickFirst, axes, candidates, next, previousIndices, previousEndpoints, separation, closeAxes, scores },
    If[ paths === {}, Return[ {} ] ];
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIndex = AssociationThread[ vertices, Range @ Length @ vertices ];
    distanceFunction = "AxisDistance" /. opts /. "AxisDistance" -> "MinEndpoint";
    minSeparation = "MinSeparation" /. opts /. "MinSeparation" -> Automatic;
    minSeparation = Replace[ minSeparation, Automatic -> (Length[ First[ paths ] ] - 1) / 2 ];
    thickness = "AxisThickness" /. opts /. "AxisThickness" -> 0;
    pickFirst = ! TrueQ[ "RandomPick" /. opts /. "RandomPick" -> False ];
    pick = If[ pickFirst, First, RandomChoice ];
    axes = { pick[ paths ] };
    previousIndices = Lookup[ vertexIndex, axes[[ 1 ]] ];
    previousEndpoints = { vertexIndex[ axes[[ 1, 1 ]] ], vertexIndex[ axes[[ 1, -1 ]] ] };
    candidates = Complement[ paths, axes ];
    While[ candidates =!= {},
      scores = Switch[ distanceFunction,
        "MinEndpoint",
          ( Min[
              distMatrix[[ vertexIndex[ #[[ 1 ]] ], previousEndpoints ]],
              distMatrix[[ vertexIndex[ #[[ -1 ]] ], previousEndpoints ]]
            ] & ) /@ candidates,
        "Hausdorff",
          ( p |-> HausdorffDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates,
        "Separation",
          ( p |-> MinimalSeparationDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates,
        _, Return[ axes ]
      ];
      separation = Max[ scores ];
      If[ separation < minSeparation, Break[] ];
      next = pick[ candidates[[ Flatten @ Position[ scores, separation ] ]] ];
      closeAxes = If[ thickness == 0, { next },
        Select[ candidates, HausdorffDistance[ distMatrix, Lookup[ vertexIndex, # ], Lookup[ vertexIndex, next ] ] <= thickness & ]
      ];
      axes = Join[ axes, closeAxes ];
      previousIndices = Union[ previousIndices, Flatten[ Lookup[ vertexIndex, # ] & /@ closeAxes ] ];
      previousEndpoints = Union[ previousEndpoints, Flatten[ { vertexIndex[ #[[ 1 ]] ], vertexIndex[ #[[ -1 ]] ] } & /@ closeAxes ] ];
      candidates = Complement[ candidates, closeAxes ];
    ];
    axes
  ]

(* ============================================================
   FindOrthogonalFrame -- orthogonal-frame search on GeodesicGraph
   ============================================================

   FindOrthogonalFrame[g, c] returns a list of axes mutually perpendicular
   at the centre c.  Each axis is wrapped as InfraSegment[{path}] (one
   realisation: the maximal metric line through c with c strictly interior;
   no rays).  Algorithm: build GeodesicGraph[g, c] (BFS DAG from c);
   enumerate candidate lines via antipodal DAG-vertex pairs; DFS through
   the choice tree, filtering the DAG by perpendicularity at each step.

   Math conditions (encoded by the helpers below):

   - Half-axis from c: a directed path c -> v in GeodesicGraph[g, c],
     equivalently a geodesic from c.  By BFS-DAG construction every prefix
     is a geodesic (= metric-additivity automatic).

   - Line through c, with halves (h+, h-):  d_g(end(h+), end(h-)) =
     depth(end(h+)) + depth(end(h-)), with both halves non-trivial.  The
     axis vertex sequence is Join[Reverse[h-], Rest[h+]] with c at position
     |h-|.  A vertex with no through-line (a corner) yields no frame.

   - Mutual perpendicularity at c of axes A, B: for every vertex w of B,
     c's axis-index on A is among w's tied closest positions on A
     (membership test, not "the first tied projection equals c"); and
     symmetrically for vertices of A on B.  The forward direction is
     handled by filtering the DAG via restrictDagToCenter; the reverse
     direction is checked per candidate axis.

   - Half-axis depth filter ("AxisLength"): each half-axis must have depth
     in the spec's range.  Spec forms: All (any), n (exactly n; the "local
     axis filling the radius-n ball"), UpTo[n] (<= n), {min, max} (range).
     Setting "AxisLength" -> n is the natural discrete-dimension probe --
     count axes vs n to read off scale-dependent dimension.

   - Frame size filter ("AxisCount"): Automatic (default) records only at
     DFS leaves where no perpendicular axis can be added (saturated
     frames); k records frames of exactly k axes; UpTo[k] records when
     either k axes reached or saturation; All records at every depth >= 1.

   See Wiki/Concepts/APIConventions.md "Search semantics" for the
   project-wide rules on the count triple, Method, BranchSampleSize, and
   structural-count options. *)


(* allHalfAxes -- every directed c -> v path in dag (FindPath All); v at
   any depth, including c itself (the trivial half-axis (c)). *)

allHalfAxes[ dag_Graph, c_ ] :=
  Catenate[ FindPath[ dag, c, #, Infinity, All ] & /@ VertexList[ dag ] ]


(* enumerateAxes -- every candidate line through c with each half-axis of
   depth >= minLength.  Pairs half-axes (h+, h-) with antipodal endpoints
   in g.  Deduplication on the orientation-canonical vertex sequence
   (lex-min of axis and its reverse).  Setting minLength = AxisLength
   yields "local axes filling the ball of radius AxisLength" (axes that
   touch the depth boundary). *)

enumerateAxes[ g_Graph, dag_Graph, c_, minLength_Integer ] :=
  Module[ { dist, halvesByEnd, vertsAtDepth, lines },
    dist = AssociationThread[ VertexList[ dag ], GraphDistance[ dag, c, # ] & /@ VertexList[ dag ] ];
    halvesByEnd = GroupBy[ allHalfAxes[ dag, c ], Last ];
    vertsAtDepth = Select[ VertexList[ dag ], dist[ # ] >= minLength & ];
    lines = Catenate @ Map[
      pair |-> Flatten[
        Outer[
          { hPos, hNeg } |-> Join[ Reverse @ hNeg, Rest @ hPos ],
          halvesByEnd[ pair[[ 1 ]] ], halvesByEnd[ pair[[ 2 ]] ], 1
        ], 1
      ],
      Select[ Subsets[ vertsAtDepth, { 2 } ],
        pair |-> GraphDistance[ g, pair[[ 1 ]], pair[[ 2 ]] ] === dist[ pair[[ 1 ]] ] + dist[ pair[[ 2 ]] ]
      ]
    ];
    DeleteDuplicatesBy[ lines, First @ Sort[ { #, Reverse @ # } ] & ]
  ]


(* axisSortKey -- lex sort key on axes.  All axes are full lines now:
   first key is -Length[axis] (longer first), then orientation-canonical
   sequence (deterministic tiebreak). *)

axisSortKey[ axis_List ] :=
  { -Length[ axis ], Min[ axis, Reverse @ axis ] }


(* projectsToCenterQ -- does w project to c on axis?  Membership test:
   c's axis-index must be among w's tied closest positions on the axis
   (= the "closest-to-centre" tie among w's projections is c itself).
   This is the sel-agnostic perpendicularity condition; SelectCoordinate
   only governs OrthogonalCoordinates' output reduction, not the
   perpendicularity filter. *)

projectsToCenterQ[ g_Graph, axis_, c_, w_ ] :=
  MemberQ[ axisLayerIndex[ g, axis, w ], First @ axisLayerIndex[ g, axis, c ] ]


(* restrictDagToCenter -- keep only DAG vertices that project to c on axis
   (forward perpendicularity filter, applied between DFS levels). *)

restrictDagToCenter[ g_Graph, dag_Graph, axis_List, c_ ] :=
  Subgraph[ dag, Select[ VertexList[ dag ], projectsToCenterQ[ g, axis, c, # ] & ] ]


(* canonicalFrame -- orientation-canonical form of a list of axes for
   deduplication: each axis lex-min'd against its reverse, then the whole
   list sorted. *)

canonicalFrame[ axes_List ] := Sort[ First @ Sort[ { #, Reverse @ # } ] & /@ axes ]


(* orthogonalFrameDFS -- depth-first traversal of the (axis 1, axis 2, ...)
   choice tree.  Filters the DAG by forward perpendicularity at each step;
   verifies reverse perpendicularity per candidate.  Records frames per
   axisCountSpec (Automatic = saturated leaves, k_Integer = exactly k,
   UpTo[k] = up to k or saturation, All = every level >= 1).  Bails via
   Throw once maxFrames distinct frames have been collected. *)

recordFrameQ[ Automatic ][ len_, vAxes_ ] := vAxes === {} && len > 0
recordFrameQ[ All       ][ len_, _ ]       := len > 0
recordFrameQ[ n_Integer ][ len_, _ ]       := len === n
recordFrameQ[ UpTo[ n_ ] ][ len_, vAxes_ ] := len === n || (vAxes === {} && len > 0)

recurseDFSQ[ Automatic ][ _, vAxes_ ]       := vAxes =!= {}
recurseDFSQ[ All       ][ _, vAxes_ ]       := vAxes =!= {}
recurseDFSQ[ n_Integer ][ len_, vAxes_ ]    := len < n && vAxes =!= {}
recurseDFSQ[ UpTo[ n_ ] ][ len_, vAxes_ ]   := len < n && vAxes =!= {}

orthogonalFrameDFS[ g_Graph, c_, fullDag_Graph, axisCountSpec_, minLength_, sampleSize_, maxFrames_ ] :=
  Module[ { frames = {}, canonForms = {}, dfs },
    dfs[ dag_, currentAxes_ ] :=
      Module[ { len, axisCands, validAxes, sortedAxes, sampledAxes, canon },
        len = Length[ currentAxes ];
        axisCands = enumerateAxes[ g, dag, c, minLength ];
        validAxes = Select[ axisCands,
          cand |-> AllTrue[ currentAxes,
            prev |-> AllTrue[ prev, w |-> projectsToCenterQ[ g, cand, c, w ] ]
          ]
        ];
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
            RandomSample[ sortedAxes, sampleSize ]
          ];
          Scan[
            axis |-> dfs[ restrictDagToCenter[ g, dag, axis, c ], Append[ currentAxes, axis ] ],
            sampledAxes
          ]
        ]
      ];
    Catch[ dfs[ fullDag, {} ] ];
    frames
  ]


(* frameSortKey -- frames ranked for "Exhaustive" mode by:
   (1) -(axis count),
   (2) -(total length across all axes),
   (3) canonical-id.
   More axes beats fewer; among same-axis-count frames, more total
   vertices breaks ties; canonical-id is the deterministic final tiebreak. *)

frameSortKey[ frame_List ] :=
  { -Length[ frame ], -Total[ Length /@ frame ], canonicalFrame[ frame ] }


Options[ FindOrthogonalFrame ] = {
  Method             -> Automatic,
  "AxisLength"       -> All,
  "AxisCount"        -> Automatic,
  "BranchSampleSize" -> All
};


(* parseAxisLengthSpec -- normalise "AxisLength" spec to a {min, max} pair.
   Accepted forms:
     All              -> {1, Infinity}        (any depth, default)
     n_Integer        -> {n, n}               (depth exactly n; local axis
                                              filling the radius-n ball)
     UpTo[n]          -> {1, n}               (depth at most n)
     {min, max}       -> {min, max}           (depth in the explicit range) *)

parseAxisLengthSpec[ All ]           := { 1, Infinity }
parseAxisLengthSpec[ n_Integer ]     := { n, n }
parseAxisLengthSpec[ UpTo[ n_ ] ]    := { 1, n }
parseAxisLengthSpec[ { min_, max_ } ] := { min, max }


(* resolveSearchMethod -- shared resolver for Method -> Automatic.
   Default is "Exhaustive" in all cases (vertex and InfraPoint centres).
   Users opt into "Greedy" explicitly when full enumeration is too costly. *)

resolveSearchMethod[ opts_List ] :=
  Replace[ Method /. opts /. Method -> Automatic, Automatic -> "Exhaustive" ]


(* findOrthogonalFrameCore -- runs the DFS engine; for "Exhaustive" mode
   ranks the resulting frames by frameSortKey; for "Greedy" mode keeps DFS
   order (first n leaves found, deterministic via axisSortKey). *)

findOrthogonalFrameCore[ g_Graph, c_, count_, opts_List ] /; MemberQ[ VertexList[ g ], c ] :=
  Module[ { minLength, maxDepth, dag, axisCountSpec, sampleSize, method, maxFrames, frames },
    { minLength, maxDepth } = parseAxisLengthSpec[ "AxisLength" /. opts /. "AxisLength" -> All ];
    dag           = GeodesicGraph[ g, c, "AxisLength" -> Replace[ maxDepth, Infinity -> All ] ];
    axisCountSpec = "AxisCount" /. opts /. "AxisCount" -> Automatic;
    method        = resolveSearchMethod[ opts ];
    sampleSize    = If[ method === "Greedy", All,
      "BranchSampleSize" /. opts /. "BranchSampleSize" -> All ];
    maxFrames = If[ method === "Greedy" && IntegerQ @ count, count, Infinity ];
    frames = orthogonalFrameDFS[ g, c, dag, axisCountSpec, minLength, sampleSize, maxFrames ];
    If[ method === "Exhaustive", SortBy[ frames, frameSortKey ], frames ]
  ]

(* InfraPoint multi-vertex centre: run the search per source vi and merge.
   Each frame's axes share the source as anchor (a less-general semantics
   than allowing per-axis anchors, but cleanly composes with the
   single-source DFS engine). *)

findOrthogonalFrameCore[ g_Graph, InfraPoint[ vs_List ], count_, opts_List ] :=
  Module[ { perSource, allFrames, method, maxFrames, sortedFrames },
    method     = resolveSearchMethod[ opts ];
    perSource  = Map[ findOrthogonalFrameCore[ g, #, All, opts ] &, vs ];
    allFrames  = DeleteDuplicatesBy[ Catenate @ perSource, canonicalFrame ];
    sortedFrames = If[ method === "Exhaustive", SortBy[ allFrames, frameSortKey ], allFrames ];
    maxFrames = If[ count === All, Infinity, count ];
    Take[ sortedFrames, UpTo[ maxFrames ] ]
  ]


(* ===================== FindSpanningAxes ===================== *)

(* FindSpanningAxes is the no-center form: greedy mutually-separated longest
   geodesics across the whole graph (no fixed interior point).  Reuses
   orthogonalGreedy + the legacy Hausdorff-separation options. *)

Options[ FindSpanningAxes ] = {
  "AxisDistance"  -> "MinEndpoint",
  "MinLength"     -> Automatic,
  "MinSeparation" -> Automatic,
  "AxisThickness" -> 0,
  "RandomPick"    -> False
};

FindSpanningAxes[ g_Graph, All, opts : OptionsPattern[] ] :=
  Module[ { distMatrix, minLength, paths },
    distMatrix = GraphDistanceMatrix[ g ];
    minLength = Replace[ OptionValue[ "MinLength" ], Automatic -> Max[ distMatrix ] ];
    paths = findLongestPaths[ g, All, Max[ distMatrix ] - minLength ];
    orthogonalGreedy[ g, paths, { opts } ]
  ]

FindSpanningAxes[ g_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindSpanningAxes[ g, All, opts ], UpTo[ n ] ]

FindSpanningAxes[ g_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindSpanningAxes[ g, UpTo[ n ], opts ] },
    If[ Length[ result ] >= n, Take[ result, n ], $Failed ]
  ]

(* ===================== FindOrthogonalFrame public dispatch ===================== *)

(* Vertex centre, calling triple.  count = 1 (default): one bare frame.
   count = n_Integer: list of n distinct frames or $Failed.  count = UpTo[n]:
   up to n.  count = All: every distinct frame (Method default = "Exhaustive").
   Each axis in a returned frame is wrapped as InfraSegment[{path}] (one
   realisation, the metric line through c). *)

wrapFrame[ frame_List ] := InfraSegment[ { # } ] & /@ frame

FindOrthogonalFrame[ g_Graph, c_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { result = findOrthogonalFrameCore[ g, c, 1, { opts } ] },
    If[ result =!= {}, wrapFrame @ First @ result, $Failed ]
  ]

FindOrthogonalFrame[ g_Graph, c_, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  wrapFrame /@ findOrthogonalFrameCore[ g, c, All, { opts } ]

FindOrthogonalFrame[ g_Graph, c_, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  wrapFrame /@ Take[ findOrthogonalFrameCore[ g, c, n, { opts } ], UpTo[ n ] ]

FindOrthogonalFrame[ g_Graph, c_, n_Integer, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { result = findOrthogonalFrameCore[ g, c, n, { opts } ] },
    If[ Length[ result ] >= n, wrapFrame /@ Take[ result, n ], $Failed ]
  ]

(* InfraPoint centre: singleton degenerates to single-vertex centre.  Multi-vertex
   InfraPoint dispatch builds the multi-source GeodesicGraph (handled by the core
   via GeodesicGraph[g, InfraPoint[vs]]). *)

FindOrthogonalFrame[ g_Graph, InfraPoint[ { v_ } ], rest___ ] :=
  FindOrthogonalFrame[ g, v, rest ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = findOrthogonalFrameCore[ g, ip, 1, { opts } ] },
    If[ result =!= {}, wrapFrame @ First @ result, $Failed ]
  ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], All, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  wrapFrame /@ findOrthogonalFrameCore[ g, ip, All, { opts } ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], UpTo[ n_Integer ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  wrapFrame /@ Take[ findOrthogonalFrameCore[ g, ip, n, { opts } ], UpTo[ n ] ]

FindOrthogonalFrame[ g_Graph, ip : InfraPoint[ vs_List ], n_Integer, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = findOrthogonalFrameCore[ g, ip, n, { opts } ] },
    If[ Length[ result ] >= n, wrapFrame /@ Take[ result, n ], $Failed ]
  ]


(* ===================== ResistanceCoordinates ===================== *)

(* The resistance-matching spectral embedding
       Phi(v) = ( phi_i(v) / sqrt(lambda_i) )_{i: lambda_i > 0}
   of the vertex set into R^(n - c) (c = number of connected components).
   By the spectral theorem, ||Phi(u) - Phi(v)||^2 = R(u, v), so Phi is a
   Euclidean isometry for the metric sqrt(R) -- the Klein-Randic
   embedding.  Other "Rescaling" choices give the plain Laplacian
   eigenvectors ("None") or the diffusion-map embedding ("Diffusion"
   -> t).  "Dimension" caps how many smallest non-zero modes are kept;
   Automatic = all of them.  "Origin" shifts the embedding so that the
   chosen vertex (or an InfraPoint centroid) lands at the origin. *)

resistanceEmbeddingMatrix[ g_Graph, rescaling_, dimSpec_ ] :=
  Module[ { vals, vecs, ord, tol, keep, dim, idx, weights },
    { vals, vecs } = Eigensystem[ N @ Normal @ KirchhoffMatrix[ g ] ];
    ord = Ordering[ vals ];
    { vals, vecs } = { vals[[ ord ]], vecs[[ ord ]] };
    tol = 10^-10 Max[ Abs @ vals, 1 ];
    keep = Select[ Range @ Length @ vals, vals[[ # ]] > tol & ];
    dim = Replace[ dimSpec, {
      Automatic | All :> Length[ keep ],
      UpTo[ k_Integer ] :> Min[ k, Length[ keep ] ],
      k_Integer :> Min[ k, Length[ keep ] ]
    } ];
    idx = Take[ keep, dim ];
    weights = Replace[ rescaling, {
      "ResistanceMatching" :> 1 / Sqrt[ vals[[ idx ]] ],
      "None" :> ConstantArray[ 1, Length[ idx ] ],
      ("Diffusion" -> t_) :> Exp[ -t vals[[ idx ]] ]
    } ];
    Transpose[ weights vecs[[ idx ]] ]
  ]


Options[ ResistanceCoordinates ] = {
  "Rescaling" -> "ResistanceMatching",
  "Dimension" -> Automatic,
  "Origin" -> None
};

ResistanceCoordinates[ g_Graph, opts : OptionsPattern[] ] :=
  Module[ { mat, origin, idx, originVec },
    mat = resistanceEmbeddingMatrix[ g, OptionValue[ "Rescaling" ], OptionValue[ "Dimension" ] ];
    origin = OptionValue[ "Origin" ];
    idx = AssociationThread[ VertexList[ g ], Range @ VertexCount[ g ] ];
    originVec = Switch[ origin,
      None,                  ConstantArray[ 0., Length @ First @ mat ],
      InfraPoint[ { _ } ],   mat[[ idx[ origin[[ 1, 1 ]] ] ]],
      InfraPoint[ _List ],   Mean[ mat[[ idx /@ origin[[ 1 ]] ]] ],
      _,                     mat[[ idx[ origin ] ]]
    ];
    AssociationThread[ VertexList[ g ], # - originVec & /@ mat ]
  ]

ResistanceCoordinates[ g_Graph, v_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], v ] :=
  ResistanceCoordinates[ g, opts ][ v ]

ResistanceCoordinates[ g_Graph, InfraPoint[ { v_ } ], opts : OptionsPattern[] ] :=
  ResistanceCoordinates[ g, v, opts ]

ResistanceCoordinates[ g_Graph, InfraPoint[ vs_List ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { all = ResistanceCoordinates[ g, opts ] }, all /@ vs ]
