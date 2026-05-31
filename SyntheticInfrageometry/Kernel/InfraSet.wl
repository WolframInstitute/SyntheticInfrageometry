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
