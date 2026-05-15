Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findInfraPathCore]


(* ===================== InfraPath wrapper ===================== *)

InfraPath[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPath[ _List ] ] ] :=
  InfraPath[ Flatten[ reps /. InfraPath[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraPath ===================== *)

(* A path from p1 to p2 is a simple vertex sequence (p1, ..., p2) with
   consecutive adjacency.  Wraps the Wolfram built-in FindPath with the
   project calling triple and the multi-anchor spread.  InfraPathQ \supset
   InfraSegmentQ \supset InfraLineQ. *)

Options[ FindInfraPath ] = { };

FindInfraPath[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPath, count,
    findInfraPathCore[ graph, ##, kspec ] &, p1, p2 ]


findInfraPathCore[ _Graph, p1_, p1_, _ ] := { }

findInfraPathCore[ graph_Graph, p1_, p2_, kspec_ ] :=
  Replace[
    FindPath[ graph, p1, p2, kspec, All ],
    Except[ _List ] -> { } ]
