Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraSet ===================== *)

(* Wrapper for an arbitrary vertex subset of the underlying graph. Coerces any
   Infra* wrapper to its underlying vertex set; rendered as a "Sets" highlight by
   InfraSceneVisualization.wl and accepted by InfraDistance. *)

InfraSet[ vs_List ] /; MemberQ[ vs, _InfraSet ] :=
  InfraSet[ Union @@ Replace[ vs, s_InfraSet :> s[ "Vertices" ], {1} ] ]

(* InfraPoint is special: each realisation IS a vertex (possibly a list vertex
   label like {i,j}), so the first argument is already the vertex list -- no
   flattening needed. The trailing ___ absorbs the optional weight list of the
   canonical weighted form InfraPoint[vs, weights]. *)
InfraSet[ InfraPoint[ vs_List, ___ ] ] :=
  InfraSet[ Sort @ DeleteDuplicates @ vs ]

(* All other Infra* container wrappers have realisations that are vertex-lists
   (InfraBall, InfraShell, InfraSegment, ...): rs = {{vs_1}, {vs_2}, ...}.
   Flatten one level to union all vertex-sets into a flat list. *)
InfraSet[ wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSet[ Sort @ DeleteDuplicates @ Flatten[ rs, 1 ] ]

InfraSet[ vs_List ][ "Vertices" ] := vs
InfraSet[ vs_List ][ "Length" ]   := Length[ vs ]

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
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

(* Bouncing wavefront from a source, as a pure vertex-set foliation.  Each vertex
   u of the front S_i advances one step OUTWARD in the geodesic spray of the
   k-th-predecessor shell R = S_{i-k}: to the neighbours v with d(R,v)=d(R,u)+1;
   if u has no such outward neighbour it is KEPT.  The keep + trailing reference
   is the momentum: the front expands as a thin shell, and where it can advance no
   farther it stalls until the trailing shell R catches up, then "outward from R"
   points back and the front REFLECTS -- a breathing shell on a closed map, a
   sloshing band on a graph with boundary.  Because every u contributes >= 1
   vertex the front never empties (infinite, eventually periodic), unlike the
   metric-sphere shell FindInfraShell which dies at the eccentricity.  k (default
   1) sets the dwell at each turning point; the first fold-back tracks the
   graph injectivity radius (the non-contractible-systole question, see
   Wiki/Concepts/SprayTransverseGrowth.md).  Returns
   { InfraSet[S_0], ..., InfraSet[S_steps] }; origin is a single vertex (a bare
   label, including a list-vertex {i, j}) or an InfraPoint[{...}] multi-source. *)

FindAdvancingInfraFront[ graph_Graph, origin_, steps_Integer, k_Integer : 1 ] :=
  With[
    { vl  = VertexList[ graph ],
      src = Replace[ origin, { InfraPoint[ vs_List, ___ ] :> vs, v_ :> { v } } ] },
    { adj  = AssociationMap[ AdjacencyList[ graph, # ] &, vl ],
      vidx = AssociationThread[ vl, Range[ Length @ vl ] ],
      dm   = GraphDistanceMatrix[ graph ] },
    { step = hist |-> With[
        { cur = Last @ hist, ref = hist[[ Max[ 1, Length @ hist - k ] ]] },
        { dref = AssociationThread[ vl, Min /@ Transpose[ dm[[ Lookup[ vidx, ref ] ]] ] ] },
        Append[ hist, DeleteDuplicates @ Catenate[
          ( u |-> With[ { out = Select[ adj @ u, dref[ # ] == dref[ u ] + 1 & ] },
              If[ out === { }, { u }, out ] ] ) /@ cur ] ] ] },
    InfraSet /@ Nest[ step, { src }, steps ]
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
