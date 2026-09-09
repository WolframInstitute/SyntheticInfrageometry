Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* the set instance is gone: a set IS the multiset <| v -> m |>, key-sorted so two built by different routes compare SameQ, with a List its uniform-weight sugar.  Keys is the support, Length the size, and Merge / KeyMap / KeySelect the algebra.  Everything below returns one.
   The head is gone outright, scene language included: every other Infra head names a construction and survives as its token, but a literal vertex set is dispatched by shape *)


(* ===================== FindInfraEquidistantSet ===================== *)

(* { v : d(p1, v) == ... == d(pn, v) }, the intersection of the n-1 consecutive bisectors Bis(p_i, p_{i+1}); the window thickens each to lo <= d(p_i, v) - d(p_{i+1}, v) <= hi *)

FindInfraEquidistantSet[ graph_Graph, pts_List ] :=
  FindInfraEquidistantSet[ graph, pts, { 0, 0 } ]

FindInfraEquidistantSet[ graph_Graph, pts_List, { lo_Integer, hi_Integer } ] /; Length[ pts ] >= 2 :=
  With[
    { rows  = GraphDistance[ graph, # ] & /@ pts },
    { diffs = Transpose @ MapThread[ Subtract, { Most[ rows ], Rest[ rows ] } ] },
    toDensity[ graph, Pick[ VertexList[ graph ], AllTrue[ #, lo <= # <= hi & ] & /@ diffs ] ]
  ]

FindInfraEquidistantSet[ graph_Graph, pts_List /; Length[ pts ] <= 1, { _Integer, _Integer } ] :=
  toDensity[ graph, VertexList[ graph ] ]


(* ===================== FindAdvancingInfraFront ===================== *)

(* each vertex u of the front S_i steps one shell outward from S_{i-1} -- to the neighbours v with d(S_{i-1}, v) = d(S_{i-1}, u) + 1 -- and reflects where there is no outward neighbour, stepping back to a neighbour at d(u) - 1.
   The state is the pair (S_{i-1}, S_i), so this is a NestList on consecutive fronts: the discrete second-order (wave-equation) form, momentum carried as the trailing front. *)

FindAdvancingInfraFront[ graph_Graph, origin_, steps_Integer ] :=
  With[
    { vl  = VertexList[ graph ],
      src = infraPointVertices[ graph, origin ] },
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
    toDensity[ graph, # ] & /@ NestList[ step, { src, src }, steps ][[ All, 2 ]]
  ]


(* ===================== InfraBoundary / InfraInterior ===================== *)


InfraBoundary::badmethod = "Method `1` is not supported by InfraBoundary.";
InfraInterior::badmethod = "Method `1` is not supported by InfraInterior.";

Options[ InfraBoundary ] = { Method -> "Combinatorial" };
Options[ InfraInterior ] = { Method -> "Combinatorial" };

InfraBoundary[ g_Graph, s_, OptionsPattern[] ] :=
  With[ { vs = infraSetVertices[ g, s ] },
    Switch[ methodName @ OptionValue[ Method ],
      "Combinatorial", toDensity[ g, GraphBoundary[ g, vs ] ],
      "Alexandrov",    toDensity[ g, TopologicalBoundary[
        BallTopology[ g, Lookup[ methodOptions @ OptionValue[ Method ], "Radius", 1 ] ], vs ] ],
      _, Message[ InfraBoundary::badmethod, OptionValue[ Method ] ]; $Failed
    ]
  ]

InfraInterior[ g_Graph, s_, OptionsPattern[] ] :=
  With[ { vs = infraSetVertices[ g, s ] },
    Switch[ methodName @ OptionValue[ Method ],
      "Combinatorial", toDensity[ g, GraphInterior[ g, vs ] ],
      "Alexandrov",    toDensity[ g, TopologicalInterior[
        BallTopology[ g, Lookup[ methodOptions @ OptionValue[ Method ], "Radius", 1 ] ], vs ] ],
      _, Message[ InfraInterior::badmethod, OptionValue[ Method ] ]; $Failed
    ]
  ]


(* a bare vertex list is the uniform multiset, so it is its own support; every other shape and every surviving wrapper goes through infraVertexSet *)

infraSetVertices[ g_Graph, s_ ] :=
  Which[ pointQ[ g, s ], { s }, ListQ[ s ], s, True, infraVertexSet @ s ]


(* ===================== InfraVolume ===================== *)


InfraVolume::badmeasure = "Measure `1` is not supported by InfraVolume; use \"FullCount\", \"WithoutBoundary\", \"HalfBoundary\", or \"Boundary\".";

(* the measures of Infrageometry's BallVolumes on an arbitrary set S with boundary dS = GraphBoundary[g, S]:
   "FullCount" = |S|, "WithoutBoundary" = |S| - |dS|, "HalfBoundary" = |S| - |dS|/2, and "Boundary" = |dS| itself *)
Options[ InfraVolume ] = { "Measure" -> "FullCount", Method -> "Combinatorial" };

(* line-like objects realise the union of their walks as path graphs -- only their own consecutive edges, so distinct lines are not joined and a line never gains the chords of its induced subgraph.  A vertex is then interior iff every g-edge at it is a line edge, so a 1-D curve has nearly empty interior *)
InfraVolume[ g_Graph, w : ( _Graph | { __Graph } ), opts : OptionsPattern[] ] :=
  InfraVolume[ g, InfraLine[ infraSpread @ w ], opts ]

InfraVolume[ g_Graph, (InfraLine | InfraSegment | InfraRay)[ walks_List ], opts : OptionsPattern[] ] :=
  With[
    { h = Graph[ Union @@ walks,
        DeleteDuplicates[ Sort /@ Catenate[ (UndirectedEdge @@@ Partition[ #, 2, 1 ] &) /@ walks ] ] ] },
    Switch[ OptionValue[ "Measure" ],
      "FullCount",       VertexCount[ h ],
      "WithoutBoundary", Length @ GraphInterior[ g, h ],
      "HalfBoundary",    VertexCount[ h ] - Length[ GraphBoundary[ g, h ] ] / 2,
      "Boundary",        Length @ GraphBoundary[ g, h ],
      _, Message[ InfraVolume::badmeasure, OptionValue[ "Measure" ] ]; $Failed
    ]
  ]

InfraVolume[ g_Graph, s_, opts : OptionsPattern[] ] :=
  With[ { vs = infraSetVertices[ g, s ] },
    Switch[ OptionValue[ "Measure" ],
      "FullCount",       Length[ vs ],
      "WithoutBoundary", Length[ InfraInterior[ g, vs, Method -> OptionValue[ Method ] ] ],
      "HalfBoundary",    Length[ vs ] - Length[ InfraBoundary[ g, vs, Method -> OptionValue[ Method ] ] ] / 2,
      "Boundary",        Length[ InfraBoundary[ g, vs, Method -> OptionValue[ Method ] ] ],
      _, Message[ InfraVolume::badmeasure, OptionValue[ "Measure" ] ]; $Failed
    ]
  ]

