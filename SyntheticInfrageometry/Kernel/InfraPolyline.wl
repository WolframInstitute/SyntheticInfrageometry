Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[polylineToVertexSeqs]
PackageScope[polylineToVertexSeq]
PackageScope[polylineToKnotVertices]
PackageScope[polylineToKnots]


(* ===================== InfraPolyline wrapper ===================== *)


InfraPolyline[ reps_List ][ "Length" ] :=
  Replace[ reps,
    { { }              -> 0,
      segs : { _InfraSegment .. } :> Total[ ( Length[ #[[ 1, 1 ]] ] - 1 ) & /@ segs ] },
    { 1 } ]

InfraPolyline[ reps_List ][ "Knots" ] :=
  Map[ poly |-> ( InfraPoint /@ polylineToKnots[ poly ] ), reps ]
(* ===================== FindInfraPolylineSubdivision ===================== *)

(* the fewest geodesic legs with knots on the walk, each leg a shortest path of length <= MaxLength *)

Options[ FindInfraPolylineSubdivision ] = { "MaxLength" -> Infinity };

FindInfraPolylineSubdivision[ _Graph, path_List, OptionsPattern[] ] /; Length[ path ] < 2 :=
  InfraPolyline[ { { } } ]

FindInfraPolylineSubdivision[ graph_Graph, path_List, OptionsPattern[] ] :=
  Module[ { maxLength = OptionValue[ "MaxLength" ], n = Length[ path ],
            knots = { 1 }, last = 1, d },
    Do[
      d = GraphDistance[ graph, path[[ last ]], path[[ i ]] ];
      If[ d > maxLength || i - last != d,
        AppendTo[ knots, i - 1 ];
        last = i - 1
      ],
      { i, 2, n } ];
    AppendTo[ knots, n ];
    InfraPolyline[ { MapThread[
      { a, b } |-> InfraSegment[ { path[[ a ;; b ]] } ],
      { Most @ knots, Rest @ knots } ] } ]
  ]


(* ===================== polylineToVertexSeqs ===================== *)

(* consecutive legs share their endpoint, so Rest drops the duplicate when joining *)

polylineToVertexSeqs[ reps_List ] := polylineToVertexSeq /@ reps

polylineToVertexSeq[ { } ] := { }
polylineToVertexSeq[ segs : { _InfraSegment .. } ] :=
  Fold[ Join[ #1, Rest @ #2[[ 1, 1 ]] ] &, segs[[ 1, 1, 1 ]], Rest @ segs ]


(* ===================== polylineToKnotVertices ===================== *)

(* the knots are { First[path_1], Last[path_1], ..., Last[path_k] } *)

polylineToKnotVertices[ reps_List ] := polylineToKnots /@ reps

polylineToKnots[ { } ] := { }
polylineToKnots[ segs : { _InfraSegment .. } ] :=
  Prepend[ Last[ #[[ 1, 1 ]] ] & /@ segs, First @ segs[[ 1, 1, 1 ]] ]


(* ===================== InfraPolylineQ ===================== *)

(* every leg a geodesic in graph, consecutive legs sharing their endpoint *)

InfraPolylineQ[ graph_Graph, InfraPolyline[ reps_List ] ] :=
  AllTrue[ reps, InfraPolylineQ[ graph, # ] & ]

InfraPolylineQ[ _Graph, { } ] := True
InfraPolylineQ[ graph_Graph, poly : { _InfraSegment .. } ] :=
  AllTrue[ poly, InfraSegmentQ[ graph, #[[ 1, 1 ]] ] & ] &&
  AllTrue[ Partition[ poly, 2, 1 ],
    pair |-> Last[ pair[[ 1, 1, 1 ]] ] === First[ pair[[ 2, 1, 1 ]] ] ]

InfraPolylineQ[ _Graph, _ ] := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraPolyline[ path_, opts___Rule ] ] :=
  capBranches[
    { FindInfraPolylineSubdivision[ graph, path,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraPolylineSubdivision ] ] ][[ 1, 1 ]] },
    extractBranches[ { opts } ] ]
