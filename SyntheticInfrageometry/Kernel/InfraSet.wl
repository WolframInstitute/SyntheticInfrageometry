Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraSet ===================== *)

(* Wrapper for an arbitrary vertex subset of the underlying graph. Coerces any
   Infra* wrapper to its underlying vertex set; rendered as a "Sets" highlight by
   InfraSceneVisualization.wl and accepted by InfraDistance. *)

(* idempotency: re-wrapping a set is the identity *)
InfraSet[ inner_InfraSet ] := inner

(* canonical form: a set is sorted and duplicate-free.  Guarded against the
   Infra* payload rules below (and against itself) so it fires once, and
   against PATTERN arguments: DownValues evaluate patterns, so without the
   FreeQ guard this rule would rewrite a rule-author's InfraSet[{v_, ___}]
   into InfraSet[{___, v_}] and silently bind the wrong element. *)
InfraSet[ vs_List ] /;
    FreeQ[ vs, _Blank | _BlankSequence | _BlankNullSequence | _Pattern ] &&
    ! MemberQ[ vs, _InfraPoint | _InfraSet | _InfraEffectivePoint ] &&
    ( ! DuplicateFreeQ[ vs ] || vs =!= Sort[ vs ] ) :=
  InfraSet[ Sort @ DeleteDuplicates @ vs ]

InfraSet[ vs_List ] /; MemberQ[ vs, _InfraSet ] :=
  InfraSet[ Union @@ Replace[ vs, s_InfraSet :> s[ "Vertices" ], {1} ] ]

(* Points are special: an atom IS a vertex (possibly a list label like {i,j}),
   so no flattening applies.  A list of atoms is the family layer written out;
   a effective point contributes its support -- a set discards the measure. *)
InfraSet[ InfraPoint[ v_ ] ] := InfraSet[ { v } ]
InfraSet[ list : { __InfraPoint } ] := InfraSet[ Sort @ DeleteDuplicates[ #[[ 1 ]] & /@ list ] ]
InfraSet[ InfraEffectivePoint[ m_Association ] ] := InfraSet[ Sort @ Keys @ m ]

(* A realisation may be a geodesic DAG instead of a vertex list -- the compact
   InfraSegment form FindInfraSegment returns by default.  Read the vertices off
   the DAG (VertexList == MetricInterval), never by enumerating the geodesic
   family, which is exponential in general. *)
InfraSet[ InfraSegment[ dag_Graph ] ] := InfraSet[ VertexList @ dag ]

(* All other Infra* container wrappers have realisations that are vertex-lists
   (InfraBall, InfraShell, InfraSegment, ...): rs = {{vs_1}, {vs_2}, ...}.
   Flatten one level to union all vertex-sets into a flat list; a DAG sitting in
   a realisation slot contributes its vertices. *)
InfraSet[ wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSet[ Sort @ DeleteDuplicates @ Flatten[ Replace[ rs, d_Graph :> VertexList[ d ], { 1 } ], 1 ] ]

InfraSet[ vs_List ][ "Vertices" ] := vs
InfraSet[ vs_List ][ "Weights" ]  := ConstantArray[ 1, Length @ vs ]
InfraSet[ vs_List ][ "Length" ]   := Length[ vs ]

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
(* synthetic-invariant accessors: one row per vertex -- the rectangular,
   stats-ready collection form of the InfraPoint atom readouts. *)
InfraSet[ vs_List ][ "BallVolumes", g_, rest___ ]            := BallVolumes[ g, vs, rest ]
InfraSet[ vs_List ][ "ShellAreas", g_, rest___ ]             := ShellAreas[ g, vs, rest ]
InfraSet[ vs_List ][ "LogDifferenceQuotients", g_, rest___ ] := LogDifferenceQuotients /@ BallVolumes[ g, vs, rest ]
InfraSet[ vs_List ][ "GrowthObservables", g_, rest___ ]      := VolumeGrowthObservables[ g, vs, rest ]
InfraSet[ vs_List ][ "Dimension", g_, rest___ ]              := ( #[ "BallDimension" ] & ) /@ VolumeGrowthObservables[ g, vs, rest ]
InfraSet[ vs_List ][ "ScalarCurvature", g_, rest___ ]        := ( #[ "BallScalarCurvature" ] & ) /@ VolumeGrowthObservables[ g, vs, rest ]
InfraSet[ vs_List ][ "CurvatureByRadius", g_, rest___ ]      := ( #[ "BallCurvatureByRadius" ] & ) /@ VolumeGrowthObservables[ g, vs, rest ]

InfraSet /: BallVolumes[ g_, s_InfraSet, rest___ ]             := BallVolumes[ g, s[ "Vertices" ], rest ]
InfraSet /: ShellAreas[ g_, s_InfraSet, rest___ ]              := ShellAreas[ g, s[ "Vertices" ], rest ]
InfraSet /: VolumeGrowthObservables[ g_, s_InfraSet, rest___ ] := VolumeGrowthObservables[ g, s[ "Vertices" ], rest ]

InfraSet[ vs_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraSet[ vs ] ]
InfraSet[ vs_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraSet[ vs ] ]
InfraSet[ vs_List ][ "Measure" ] := InfraMeasure[ InfraSet[ vs ] ]
InfraSet[ vs_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraSet[ vs ], Method -> "Probability" ]


(* ===================== FindInfraEquidistantSet ===================== *)

(* The equidistant set of p1, ..., pn: { v : d(p1, v) == ... == d(pn, v) }, the
   locus of common circumcenters, returned as an InfraSet.  Equal to the
   intersection of the n-1 consecutive bisectors Bis(p_i, p_{i+1}); the window
   {lo, hi} thickens each consecutive bisector to the slab lo <= d(p_i, v) -
   d(p_{i+1}, v) <= hi (for n == 2 this is exactly FindInfraBisectingHyperplane's
   slab).  Strict default {0, 0} is the order-independent perpendicular-bisector
   intersection. *)

FindInfraEquidistantSet[ graph_Graph, pts_List ] :=
  FindInfraEquidistantSet[ graph, pts, { 0, 0 } ]

FindInfraEquidistantSet[ graph_Graph, pts_List, { lo_Integer, hi_Integer } ] /; Length[ pts ] >= 2 :=
  With[
    { rows  = GraphDistance[ graph, # ] & /@ pts },
    { diffs = Transpose @ MapThread[ Subtract, { Most[ rows ], Rest[ rows ] } ] },
    InfraSet @ Pick[ VertexList[ graph ], AllTrue[ #, lo <= # <= hi & ] & /@ diffs ]
  ]

FindInfraEquidistantSet[ graph_Graph, pts_List /; Length[ pts ] <= 1, { _Integer, _Integer } ] :=
  InfraSet @ VertexList[ graph ]


(* ===================== FindAdvancingInfraFront ===================== *)

(* Bouncing wavefront from a source o, as a pure vertex-set foliation.  Each vertex
   u of the front S_i steps one shell OUTWARD from the previous front S_{i-1} -- to
   the neighbours v with d(S_{i-1}, v) = d(S_{i-1}, u) + 1 -- and where there is no
   outward neighbour it REFLECTS, stepping back to a neighbour with d = d(u) - 1
   (kept in place only in the degenerate case of neither).  The previous front is
   the momentum: the front expands as a thin metric shell and turns straight around
   at a boundary or focus, so it never empties and never dwells -- unlike the
   metric-sphere shell FindInfraShell, which dies at the eccentricity.  The state is
   the pair (S_{i-1}, S_i), so this is a NestList on consecutive-front pairs: the
   discrete second-order (wave-equation) form, momentum carried as the trailing
   front.  Returns { InfraSet[S_0], ..., InfraSet[S_steps] }; origin is a single
   vertex (a bare label, including a list-vertex {i, j}), an InfraPoint atom, an InfraSet
   multi-source. *)

FindAdvancingInfraFront[ graph_Graph, origin_, steps_Integer ] :=
  With[
    { vl  = VertexList[ graph ],
      src = infraPointVertices @ origin },
    { adj  = AssociationMap[ AdjacencyList[ graph, # ] &, vl ],
      vidx = AssociationThread[ vl, Range[ Length @ vl ] ],
      dm   = GraphDistanceMatrix[ graph ] },
    { step = pair |-> With[
        { prev = pair[[ 1 ]], cur = pair[[ 2 ]] },
        { dp = AssociationThread[ vl, Min /@ Transpose[ dm[[ Lookup[ vidx, prev ] ]] ] ] },
        { cur, DeleteDuplicates @ Catenate[
          ( u |-> With[
              { out = Select[ adj @ u, dp[ # ] == dp[ u ] + 1 & ],
                in  = Select[ adj @ u, dp[ # ] == dp[ u ] - 1 & ] },
              Which[ out =!= { }, out, in =!= { }, in, True, { u } ] ] ) /@ cur ] } ] },
    InfraSet /@ NestList[ step, { src, src }, steps ][[ All, 2 ]]
  ]


(* ===================== InfraBoundary / InfraInterior ===================== *)

(* Boundary / interior of a vertex set S (any Infra* object) in g, returned as an
   InfraSet. Method -> "Combinatorial" (default): the inner vertex boundary
   {v in S : v adjacent to V\S} and its complement S \ boundary (GraphBoundary /
   GraphInterior). Method -> {"Alexandrov", "Radius" -> r}: the two-sided
   cl(S)\int(S) and int(S) in the closed-r-ball Alexandrov topology BallTopology[g, r]
   (default r = 1). Both backends come from WolframInstitute`Infrageometry`. *)

InfraBoundary::badmethod = "Method `1` is not supported by InfraBoundary.";
InfraInterior::badmethod = "Method `1` is not supported by InfraInterior.";

Options[ InfraBoundary ] = { Method -> "Combinatorial" };
Options[ InfraInterior ] = { Method -> "Combinatorial" };

InfraBoundary[ g_Graph, s_, OptionsPattern[] ] :=
  With[ { vs = infraVertexSet @ If[ ListQ[ s ], InfraSet[ s ], s ] },
    Switch[ methodName @ OptionValue[ Method ],
      "Combinatorial", InfraSet @ GraphBoundary[ g, vs ],
      "Alexandrov",    InfraSet @ TopologicalBoundary[
        BallTopology[ g, Lookup[ methodOptions @ OptionValue[ Method ], "Radius", 1 ] ], vs ],
      _, Message[ InfraBoundary::badmethod, OptionValue[ Method ] ]; $Failed
    ]
  ]

InfraInterior[ g_Graph, s_, OptionsPattern[] ] :=
  With[ { vs = infraVertexSet @ If[ ListQ[ s ], InfraSet[ s ], s ] },
    Switch[ methodName @ OptionValue[ Method ],
      "Combinatorial", InfraSet @ GraphInterior[ g, vs ],
      "Alexandrov",    InfraSet @ TopologicalInterior[
        BallTopology[ g, Lookup[ methodOptions @ OptionValue[ Method ], "Radius", 1 ] ], vs ],
      _, Message[ InfraInterior::badmethod, OptionValue[ Method ] ]; $Failed
    ]
  ]


(* ===================== InfraVolume ===================== *)

(* InfraVolume[g, S]: the infra-observer's volume of a vertex set S (any Infra*
   object or bare vertex list), taken over the union vertex set. "Volume" picks
   the measure: "Hausdorff" (default) = |S| - |boundary| = the genuine inside
   excluding the boundary layer (same GraphInterior count as BallVolumes'
   "Measure" -> "Hausdorff"); "Counting" = |S|; "Boundary" = |boundary|. The
   boundary backend (Method -> "Combinatorial" inner vertex boundary, default, or
   {"Alexandrov", "Radius" -> r}) is shared with InfraBoundary / InfraInterior. *)

InfraVolume::badvolume = "Volume measure `1` is not supported by InfraVolume; use \"Hausdorff\", \"Counting\", or \"Boundary\".";

Options[ InfraVolume ] = { "Volume" -> "Hausdorff", Method -> "Combinatorial" };

(* Line-like objects (InfraLine / InfraSegment / InfraWalk / InfraRay) realise the
   UNION of their walks as path graphs -- only each line's own consecutive edges,
   so distinct lines are not joined and a line never gains the chords of its
   induced subgraph. The interior is then the same GraphInterior, fed this graph
   rather than a vertex list: a vertex is interior iff every g-edge at it is a
   line edge, so a 1-D curve has ~empty interior (a space-filling grid path keeps
   only its two pass-through corners). *)
InfraVolume[ g_Graph, (InfraLine | InfraSegment | InfraWalk | InfraRay)[ walks_List ], opts : OptionsPattern[] ] :=
  With[
    { h = Graph[ Union @@ walks,
        DeleteDuplicates[ Sort /@ Catenate[ (UndirectedEdge @@@ Partition[ #, 2, 1 ] &) /@ walks ] ] ] },
    Switch[ OptionValue[ "Volume" ],
      "Counting",  VertexCount[ h ],
      "Hausdorff", Length @ GraphInterior[ g, h ],
      "Boundary",  Length @ GraphBoundary[ g, h ],
      _, Message[ InfraVolume::badvolume, OptionValue[ "Volume" ] ]; $Failed
    ]
  ]

InfraVolume[ g_Graph, s_, opts : OptionsPattern[] ] :=
  With[ { vs = infraVertexSet @ If[ ListQ[ s ], InfraSet[ s ], s ] },
    Switch[ OptionValue[ "Volume" ],
      "Counting",  Length[ vs ],
      "Hausdorff", Length[ InfraInterior[ g, InfraSet[ vs ], Method -> OptionValue[ Method ] ][ "Vertices" ] ],
      "Boundary",  Length[ InfraBoundary[ g, InfraSet[ vs ], Method -> OptionValue[ Method ] ][ "Vertices" ] ],
      _, Message[ InfraVolume::badvolume, OptionValue[ "Volume" ] ]; $Failed
    ]
  ]
