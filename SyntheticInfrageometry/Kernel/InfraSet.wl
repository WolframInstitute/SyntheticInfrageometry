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

(* Advancing ORIENTED wavefront from a source.  The state is a set of geodesic
   walk-TAILS; the head of each tail is a front vertex and the rest is the recent
   history that orients the next step (the momentum a bare vertex set, and hence
   the metric-sphere shell, lacks).  A tail (..., a, b) advances b -> c, and the
   visible front S_i is the set of heads.  Two orthogonal option axes control
   which c are admissible; the default ({1, None}) is the plain non-backtracking
   front c in N(b) \ {a}, which never empties (eventually periodic) and so
   outlives the metric sphere -- on a triangle {A,B,C} from A it runs
   {A},{B,C},{B,C},{A},... (step 2 turns forward along BC, does NOT fall back to
   A; only step 3 closes the loop).

   "SelfAvoidanceDepth" -> n (default 1) -- the memory axis: c may not be any of
   the last n vertices of the tail.  n = 1 is non-backtracking; larger n forbids
   closing any cycle of length <= n + 1 (n = 2 makes a triangle die at step 3
   rather than circulate); n = Infinity is a fully self-avoiding front.

   "GeodesicSprayDepth" -> None (default) | k_Integer | Infinity -- the transverse
   axis: in the geodesic spray of a reference vertex p, every step b -> c is
   outward / inward / transverse by d(p,c) - d(p,b) in {+1, -1, 0}; this option
   forbids the transverse (0) steps.  None imposes nothing; an integer k takes
   p = the k-th predecessor (a LOCAL moving frame -- delays but does not stop
   surface flooding); Infinity takes p = origin (a GLOBAL radial layering -- the
   wave stays a band forever, never flooding).  k = 1 with the immediate
   predecessor is the strictest local frame.

   Returns the foliation { InfraSet[S_0], ..., InfraSet[S_steps] }.  origin is a
   single vertex (a bare label, including a list-vertex {i, j}); an
   InfraPoint[{v1, ..., vk}] seeds a multi-source front. *)

Options[ FindAdvancingInfraFront ] = { "SelfAvoidanceDepth" -> 1, "GeodesicSprayDepth" -> None };

FindAdvancingInfraFront[ graph_Graph, origin_, steps_Integer, OptionsPattern[ ] ] :=
  With[
    { vl  = VertexList[ graph ],
      src = Replace[ origin, { InfraPoint[ vs_List, ___ ] :> vs, v_ :> { v } } ],
      n   = OptionValue[ "SelfAvoidanceDepth" ],
      ref = OptionValue[ "GeodesicSprayDepth" ] },
    { adj  = AssociationMap[ AdjacencyList[ graph, # ] &, vl ],
      vidx = AssociationThread[ vl, Range[ Length @ vl ] ],
      dm   = GraphDistanceMatrix[ graph ] },
    { dist   = dm[[ vidx[ #1 ], vidx[ #2 ] ]] &,
      dsrc   = AssociationThread[ vl, Min /@ Transpose[ dm[[ Lookup[ vidx, src ] ]] ] ],
      retain = Max[ n /. Infinity -> steps, If[ IntegerQ @ ref, ref, 0 ] ] + 1 },
    { step = tails |-> DeleteDuplicates @ Catenate[
        ( t |-> With[
            { b = Last @ t, pred = If[ Length @ t >= 2, t[[ -2 ]], None ] },
            { fwd    = If[ pred === None, adj[ b ], DeleteCases[ adj[ b ], pred ] ],
              memSet = If[ Length @ t >= 2, Take[ Most @ t, - Min[ n /. Infinity -> Length @ t, Length @ t - 1 ] ], { } ],
              refv   = If[ IntegerQ @ ref, t[[ Max[ 1, Length @ t - ref ] ]], ref ] },
            { nxt = If[ fwd === { }, { pred },
                Select[ fwd, ! MemberQ[ memSet, # ] && Which[
                    ref === None,     True,
                    ref === Infinity, dsrc[ # ] =!= dsrc[ b ],
                    True,             dist[ refv, # ] =!= dist[ refv, b ] ] & ] ] },
            ( With[ { w = Append[ t, # ] }, If[ Length @ w > retain, Rest @ w, w ] ] & ) /@ nxt ] )
          /@ tails ] },
    InfraSet[ DeleteDuplicates[ Last /@ # ] ] & /@ NestList[ step, List /@ src, steps ]
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
