Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[infraSpread]
PackageScope[infraWrappedQ]
PackageScope[infraCapBy]
PackageScope[infraSpreadAndCartesian]


(* ===================== Multi-realisation wrappers ===================== *)

(* InfraPoint, InfraSegment, InfraShell, InfraPlane, InfraCircle each carry a
   list of realisations when applied to a single _List argument.  This file
   attaches the wrapper behaviour to those heads:

     - auto-flatten nested same-head wrappers,
     - Part returns a wrapped sub-list (InfraPoint[{a, b, c}][[1]] is
       InfraPoint[{a}], not the bare vertex a),
     - property accessors ["Realisations"] / ["Length"] / ["Expand"]
       / ["First"] for the standard read-out.

   The wrapper rule fires only on the single-_List-arg form, so it does NOT
   collide with the scene-constructor signatures of the same heads that take
   bare or multi argument shapes (InfraPoint[v], InfraPoint[origin, dist],
   InfraSegment[p, q], InfraShell[c, r], InfraPlane[p1, p2],
   InfraCircle[c, r], ...).                                                  *)


(* ===================== Auto-flatten nested wrappers ===================== *)

InfraPoint[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPoint[ _List ] ] ] :=
  InfraPoint[ Flatten[ reps /. InfraPoint[ xs_List ] :> xs, 1 ] ]

InfraSegment[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraSegment[ _List ] ] ] :=
  InfraSegment[ Flatten[ reps /. InfraSegment[ xs_List ] :> xs, 1 ] ]

InfraShell[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraShell[ _List ] ] ] :=
  InfraShell[ Flatten[ reps /. InfraShell[ xs_List ] :> xs, 1 ] ]

InfraPlane[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPlane[ _List ] ] ] :=
  InfraPlane[ Flatten[ reps /. InfraPlane[ xs_List ] :> xs, 1 ] ]

InfraCircle[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraCircle[ _List ] ] ] :=
  InfraCircle[ Flatten[ reps /. InfraCircle[ xs_List ] :> xs, 1 ] ]


(* ===================== Part: wrapped sub-list ===================== *)

InfraPoint /: Part[ InfraPoint[ reps_List ], i_Integer ]   := InfraPoint[ { reps[[ i ]] } ]
InfraPoint /: Part[ InfraPoint[ reps_List ], spec_ ]       := InfraPoint[ reps[[ spec ]] ]

InfraSegment /: Part[ InfraSegment[ reps_List ], i_Integer ] := InfraSegment[ { reps[[ i ]] } ]
InfraSegment /: Part[ InfraSegment[ reps_List ], spec_ ]     := InfraSegment[ reps[[ spec ]] ]

InfraShell /: Part[ InfraShell[ reps_List ], i_Integer ] := InfraShell[ { reps[[ i ]] } ]
InfraShell /: Part[ InfraShell[ reps_List ], spec_ ]     := InfraShell[ reps[[ spec ]] ]

InfraPlane /: Part[ InfraPlane[ reps_List ], i_Integer ] := InfraPlane[ { reps[[ i ]] } ]
InfraPlane /: Part[ InfraPlane[ reps_List ], spec_ ]     := InfraPlane[ reps[[ spec ]] ]

InfraCircle /: Part[ InfraCircle[ reps_List ], i_Integer ] := InfraCircle[ { reps[[ i ]] } ]
InfraCircle /: Part[ InfraCircle[ reps_List ], spec_ ]     := InfraCircle[ reps[[ spec ]] ]


(* ===================== Property accessors ===================== *)

InfraPoint[ reps_List ][ "Realisations" ] := reps
InfraPoint[ reps_List ][ "Length" ]       := Length @ reps
InfraPoint[ reps_List ][ "Expand" ]       := InfraPoint[ { # } ] & /@ reps
InfraPoint[ reps_List ][ "First" ]        := First @ reps

InfraSegment[ reps_List ][ "Realisations" ] := reps
InfraSegment[ reps_List ][ "Length" ]       := Length @ reps
InfraSegment[ reps_List ][ "Expand" ]       := InfraSegment[ { # } ] & /@ reps
InfraSegment[ reps_List ][ "First" ]        := First @ reps

InfraShell[ reps_List ][ "Realisations" ] := reps
InfraShell[ reps_List ][ "Length" ]       := Length @ reps
InfraShell[ reps_List ][ "Expand" ]       := InfraShell[ { # } ] & /@ reps
InfraShell[ reps_List ][ "First" ]        := First @ reps

InfraPlane[ reps_List ][ "Realisations" ] := reps
InfraPlane[ reps_List ][ "Length" ]       := Length @ reps
InfraPlane[ reps_List ][ "Expand" ]       := InfraPlane[ { # } ] & /@ reps
InfraPlane[ reps_List ][ "First" ]        := First @ reps

InfraCircle[ reps_List ][ "Realisations" ] := reps
InfraCircle[ reps_List ][ "Length" ]       := Length @ reps
InfraCircle[ reps_List ][ "Expand" ]       := InfraCircle[ { # } ] & /@ reps
InfraCircle[ reps_List ][ "First" ]        := First @ reps


(* ===================== Internal helpers ===================== *)

(* infraWrappedQ[expr] tests whether expr is one of the four multi-realisation
   wrappers in single-_List-arg form. *)

infraWrappedQ[ ( InfraPoint | InfraSegment | InfraShell | InfraPlane | InfraCircle )[ _List ] ] := True
infraWrappedQ[ _ ] := False

(* infraSpread[anchor] is the source/endpoint-position adapter: a wrapped
   anchor is spread into its realisations, an unwrapped value becomes a
   singleton list, ready for Outer / Tuples / Cartesian iteration.          *)

infraSpread[ ( InfraPoint | InfraSegment | InfraShell | InfraPlane | InfraCircle )[ reps_List ] ] := reps
infraSpread[ other_ ] := { other }

(* infraCapBy[wrapper, count] applies the standard count semantics
   (n_Integer strict / UpTo[n] / All) to a wrapper.  Returns $Failed on
   strict-n shortfall, otherwise a same-head wrapper.                       *)

infraCapBy[ wrapper_?infraWrappedQ, All ] := wrapper

infraCapBy[ wrapper_?infraWrappedQ, UpTo[ n_Integer ] ] :=
  Head[ wrapper ][ Take[ wrapper[ "Realisations" ], UpTo[ n ] ] ]

infraCapBy[ wrapper_?infraWrappedQ, n_Integer ] /; n <= wrapper[ "Length" ] :=
  Head[ wrapper ][ Take[ wrapper[ "Realisations" ], n ] ]

infraCapBy[ _?infraWrappedQ, _Integer ] := $Failed


(* infraSpreadAndCartesian is the dispatch shell for source/endpoint anchors.
   Each anchor is spread into its realisations (or treated as a singleton if
   bare); the Cartesian product runs the single-pair core; bare-list results
   are union-deduplicated and wrapped under wrapHead.  Strict-n shortfall on
   any pair propagates as bare $Failed.                                       *)

infraSpreadAndCartesian[ wrapHead_, count_, core_, anchors__ ] :=
  With[ { results = core @@@ Tuples[ infraSpread /@ { anchors } ] },
    If[ MemberQ[ results, $Failed ],
      $Failed,
      wrapHead[ DeleteDuplicates @ Flatten[ results, 1 ] ]
    ]
  ]
