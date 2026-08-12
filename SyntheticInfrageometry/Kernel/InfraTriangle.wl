Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraTriangle wrapper ===================== *)

(* InfraTriangle[{poly}] is the unary form: poly = {seg1, seg2, seg3} is a
   closed chain of three unary InfraSegment sides; InfraTriangle[{poly1, ...}]
   is the multi-realisation form.  Same storage shape as InfraPolygon, restricted
   to three sides.  Auto-flatten on nested wrappers. *)

(* Set canonicalisation and the shared accessors come from
   defineInfraBundleRules (Tools.wl). *)

InfraTriangle[ reps_List ][ "Sides" ] := reps

InfraTriangle[ reps_List ][ "Length" ] :=
  Replace[ reps,
    { { }              -> 0,
      segs : { _InfraSegment .. } :> Total[ ( Length[ #[[ 1, 1 ]] ] - 1 ) & /@ segs ] },
    { 1 } ]

InfraTriangle[ reps_List ][ "Vertices" ] :=
  Map[ poly |-> ( InfraPoint[ { # } ] & /@ Most @ polylineToKnots[ poly ] ), reps ]
(* ===================== FindInfraTriangle ===================== *)

(* Triangle through corners a, b, c: the n = 3 case of FindInfraPolygon. *)

Options[ FindInfraTriangle ] = { Properties -> { }, Method -> "Exhaustive" };

FindInfraTriangle[ graph_Graph, vertices_List /; Length[ vertices ] === 3,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[ { core = findPolygonCore[ graph, vertices, count, opts ] },
    If[ core === $Failed, $Failed, InfraTriangle[ core ] ]
  ]


(* ===================== InfraTriangleQ ===================== *)

InfraTriangleQ[ graph_Graph, InfraTriangle[ reps_List ] ] :=
  AllTrue[ reps, InfraTriangleQ[ graph, # ] & ]

InfraTriangleQ[ graph_Graph, poly : { _InfraSegment, _InfraSegment, _InfraSegment } ] :=
  InfraPolygonQ[ graph, poly ]

InfraTriangleQ[ _Graph, _ ] := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraTriangle[ verts_List, opts___Rule ] ] :=
  capBranches[
    FindInfraTriangle[ graph, verts, All,
      Sequence @@ FilterRules[ { opts }, Options[ FindInfraTriangle ] ] ][ "Realizations" ],
    extractBranches[ { opts } ] ]
