Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[polylineToVertexSeqs]
PackageScope[polylineToVertexSeq]
PackageScope[polylineToKnotVertices]
PackageScope[polylineToKnots]


(* ===================== InfraPolyline wrapper ===================== *)

(* InfraPolyline[{poly}] is the unary form: poly = {seg1, seg2, ...} where each
   seg_i is a unary InfraSegment[{path_i}] and Last[path_i] === First[path_{i+1}]
   for consecutive legs.  InfraPolyline[{poly1, ..., polyk}] is the multi-
   realisation form.  Only auto-flatten on nested wrappers. *)

InfraPolyline[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPolyline[ _List ] ] ] :=
  InfraPolyline[ Flatten[ reps /. InfraPolyline[ xs_List ] :> xs, 1 ] ]


(* ===================== FindPolylineSubdivision ===================== *)

(* Greedy chunking of a walk into the fewest geodesic InfraSegments whose
   knots are walk-vertices, with each leg a shortest path (in graph) of
   length <= MaxLength. *)

Options[ FindPolylineSubdivision ] = { "MaxLength" -> Infinity };

FindPolylineSubdivision[ _Graph, path_List, OptionsPattern[] ] /; Length[ path ] < 2 :=
  InfraPolyline[ { { } } ]

FindPolylineSubdivision[ graph_Graph, path_List, OptionsPattern[] ] :=
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

(* Each realisation poly = {seg1, ..., segk} flattens to one concatenated
   vertex sequence; consecutive legs share their endpoint, so Rest drops
   the duplicate when joining. *)

polylineToVertexSeqs[ reps_List ] := polylineToVertexSeq /@ reps

polylineToVertexSeq[ { } ] := { }
polylineToVertexSeq[ segs : { _InfraSegment .. } ] :=
  Fold[ Join[ #1, Rest @ #2[[ 1, 1 ]] ] &, segs[[ 1, 1, 1 ]], Rest @ segs ]


(* ===================== polylineToKnotVertices ===================== *)

(* The knot set of a polyline is { First[path_1], Last[path_1], ..., Last[path_k] }:
   the start of the first leg followed by the end of every leg.  Consecutive
   legs share an endpoint so this is exactly the set of break points. *)

polylineToKnotVertices[ reps_List ] := polylineToKnots /@ reps

polylineToKnots[ { } ] := { }
polylineToKnots[ segs : { _InfraSegment .. } ] :=
  Prepend[ Last[ #[[ 1, 1 ]] ] & /@ segs, First @ segs[[ 1, 1, 1 ]] ]


(* ===================== PolylineQ ===================== *)

(* A polyline is consistent iff every leg is a geodesic in graph and
   consecutive legs share their endpoint. *)

PolylineQ[ graph_Graph, InfraPolyline[ reps_List ] ] :=
  AllTrue[ reps, PolylineQ[ graph, # ] & ]

PolylineQ[ _Graph, { } ] := True
PolylineQ[ graph_Graph, poly : { _InfraSegment .. } ] :=
  AllTrue[ poly, SegmentQ[ graph, #[[ 1, 1 ]] ] & ] &&
  AllTrue[ Partition[ poly, 2, 1 ],
    pair |-> Last[ pair[[ 1, 1, 1 ]] ] === First[ pair[[ 2, 1, 1 ]] ] ]

PolylineQ[ _Graph, _ ] := False
