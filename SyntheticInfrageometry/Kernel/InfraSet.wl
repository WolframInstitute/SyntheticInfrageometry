Package["WolframInstitute`SyntheticInfrageometry`"]


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

(* "Measure" = normalized vertex visit measure <|v -> visits/numReps|> (see VisitMeasure). *)
InfraSet[ vs_List ][ "Measure" ]  := VisitMeasure[ InfraSet[ vs ] ]
