Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findBisectingHyperplaneCore]


(* ===================== InfraPlane wrapper ===================== *)

InfraPlane[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPlane[ _List ] ] ] :=
  InfraPlane[ Flatten[ reps /. InfraPlane[ xs_List ] :> xs, 1 ] ]

InfraPlane /: Part[ InfraPlane[ reps_List ], i_Integer ] := InfraPlane[ { reps[[ i ]] } ]
InfraPlane /: Part[ InfraPlane[ reps_List ], spec_ ]     := InfraPlane[ reps[[ spec ]] ]

InfraPlane[ reps_List ][ "Realizations" ] := reps
InfraPlane[ reps_List ][ "Length" ]       := Length @ reps
InfraPlane[ reps_List ][ "Expand" ]       := InfraPlane[ { # } ] & /@ reps
InfraPlane[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindBisectingHyperplane ===================== *)

(* A bisecting hyperplane between p1 and p2 is an inclusion-minimal vertex
   subset of the (windowed) bisector { v : lo <= d(p1, v) - d(p2, v) <= hi }
   whose removal disconnects p1 from p2 in graph -- the codim-1 graph
   analog of the perpendicular bisector hyperplane.  The default window
   {0, 0} is the strict equidistant set; passing {-1, 1} thickens it to
   recover the parity-stranded middle pair when d(p1, p2) is odd. *)

FindBisectingHyperplane[ graph_Graph, p1_, p2_ ] :=
  FindBisectingHyperplane[ graph, p1, p2, { 0, 0 }, 1 ]

FindBisectingHyperplane[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) ] :=
  FindBisectingHyperplane[ graph, p1, p2, { 0, 0 }, count ]

FindBisectingHyperplane[ graph_Graph, p1_, p2_,
    window : { _Integer, _Integer } ] :=
  FindBisectingHyperplane[ graph, p1, p2, window, 1 ]

FindBisectingHyperplane[ graph_Graph, p1_, p2_,
    window : { _Integer, _Integer }, count : ( _Integer | UpTo[ _Integer ] | All ) ] :=
  infraSpreadAndCartesian[ InfraPlane, count,
    findBisectingHyperplaneCore[ graph, ##, window, count ] &, p1, p2 ]


findBisectingHyperplaneCore[ graph_Graph, p1_, p2_,
    { lo_Integer, hi_Integer }, count : ( _Integer | UpTo[ _Integer ] | All ) ] :=
  Module[ { bisector, hyperplanes },
    bisector = Pick[ VertexList[ graph ],
      MapThread[ { x, y } |-> lo <= x - y <= hi,
        { GraphDistance[ graph, p1 ], GraphDistance[ graph, p2 ] } ] ];
    hyperplanes = FindPairSeparators[ graph, Complement[ bisector, { p1, p2 } ], p1, p2 ];
    Which[
      MatchQ[ count, _Integer ] && Length[ hyperplanes ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ hyperplanes, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ hyperplanes, count ],
      count === All,                       hyperplanes,
      True,                                hyperplanes
    ]
  ]
