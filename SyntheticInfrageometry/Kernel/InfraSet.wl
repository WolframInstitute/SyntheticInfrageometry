Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraSet ===================== *)


InfraSet[ inner_InfraSet ] := inner

(* canonical form: sorted and duplicate-free.  The FreeQ guard keeps DownValues from rewriting a rule-author's pattern, which would silently bind the wrong element *)
InfraSet[ vs_List ] /;
    FreeQ[ vs, _Blank | _BlankSequence | _BlankNullSequence | _Pattern ] &&
    ! MemberQ[ vs, _InfraPoint | _InfraSet | _InfraEffectivePoint ] &&
    ( ! DuplicateFreeQ[ vs ] || vs =!= Sort[ vs ] ) :=
  InfraSet[ Sort @ DeleteDuplicates @ vs ]

InfraSet[ vs_List ] /; MemberQ[ vs, _InfraSet ] :=
  InfraSet[ Union @@ Replace[ vs, s_InfraSet :> s[ "Vertices" ], {1} ] ]

(* an atom IS a vertex (possibly a list label like {i, j}), so no flattening applies; a measure contributes its support and loses its masses *)
InfraSet[ InfraPoint[ v_ ] ] := InfraSet[ { v } ]
InfraSet[ list : { __InfraPoint } ] := InfraSet[ Sort @ DeleteDuplicates[ #[[ 1 ]] & /@ list ] ]
InfraSet[ InfraEffectivePoint[ m_Association ] ] := InfraSet[ Sort @ Keys @ m ]

(* read the vertices off the DAG (VertexList == MetricInterval), never by enumerating the geodesic family, which is exponential in general *)
InfraSet[ InfraSegment[ dag_Graph ] ] := InfraSet[ VertexList @ dag ]

InfraSet[ wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSet[ Sort @ DeleteDuplicates @ Flatten[ Replace[ rs, d_Graph :> VertexList[ d ], { 1 } ], 1 ] ]

InfraSet[ vs_List ][ "Vertices" ] := vs
InfraSet[ vs_List ][ "Weights" ]  := ConstantArray[ 1, Length @ vs ]
InfraSet[ vs_List ][ "Length" ]   := Length[ vs ]

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

(* { v : d(p1, v) == ... == d(pn, v) }, the intersection of the n-1 consecutive bisectors Bis(p_i, p_{i+1}); the window thickens each to lo <= d(p_i, v) - d(p_{i+1}, v) <= hi *)

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

(* each vertex u of the front S_i steps one shell outward from S_{i-1} -- to the neighbours v with d(S_{i-1}, v) = d(S_{i-1}, u) + 1 -- and reflects where there is no outward neighbour, stepping back to a neighbour at d(u) - 1.
   The state is the pair (S_{i-1}, S_i), so this is a NestList on consecutive fronts: the discrete second-order (wave-equation) form, momentum carried as the trailing front. *)

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


InfraVolume::badvolume = "Volume measure `1` is not supported by InfraVolume; use \"Hausdorff\", \"Counting\", or \"Boundary\".";

Options[ InfraVolume ] = { "Volume" -> "Hausdorff", Method -> "Combinatorial" };

(* line-like objects realise the union of their walks as path graphs -- only their own consecutive edges, so distinct lines are not joined and a line never gains the chords of its induced subgraph.  A vertex is then interior iff every g-edge at it is a line edge, so a 1-D curve has nearly empty interior *)
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
