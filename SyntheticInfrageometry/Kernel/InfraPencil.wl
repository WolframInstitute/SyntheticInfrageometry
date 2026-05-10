Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraPencil wrapper ===================== *)

(* InfraPencil is a multi-CONSTITUENT wrapper: its entries are themselves
   InfraRay objects, one per direction class at a base point O.  Pencil
   cardinality is the wrapper's ["Length"]; ["Rays"] flattens the
   constituent rays into a single list of vertex sequences for rendering. *)

InfraPencil[ rays_List ] /; AnyTrue[ rays, MatchQ[ InfraPencil[ _List ] ] ] :=
  InfraPencil[ Flatten[ rays /. InfraPencil[ xs_List ] :> xs, 1 ] ]

InfraPencil /: Part[ InfraPencil[ rays_List ], i_Integer ] := rays[[ i ]]
InfraPencil /: Part[ InfraPencil[ rays_List ], spec_ ]     := InfraPencil[ rays[[ spec ]] ]

InfraPencil[ rays_List ][ "Realizations" ] := rays
InfraPencil[ rays_List ][ "Length" ]       := Length @ rays
InfraPencil[ rays_List ][ "Expand" ]       := InfraPencil[ { # } ] & /@ rays
InfraPencil[ rays_List ][ "First" ]        := First @ rays
InfraPencil[ rays_List ][ "Rays" ]         := Catenate[ #[ "Realizations" ] & /@ rays ]


(* ===================== FindPencil ===================== *)

(* The pencil at vertex origin is the set of direction classes through
   origin, each realised by every maximal geodesic through origin sharing
   that direction.  FindPencil returns the pencil as InfraPencil of
   constituent InfraRays, one per direction class.  Multi-anchor origin
   (InfraPoint[{...}]) spreads Cartesian and unions the rays across
   choices.                                                              *)

findPencilCore[ graph_Graph, origin_ ] :=
  Module[ { otherVerts, allLines, byCanonical },
    otherVerts = DeleteCases[ VertexList[ graph ], origin ];
    allLines = Flatten[
      FindLine[ graph, origin, #, All ][ "Realizations" ] & /@ otherVerts,
      1
    ];
    byCanonical = GroupBy[ allLines, canonicalLine ];
    InfraRay[ DeleteDuplicatesBy[ #, canonicalLine ] ] & /@ Values[ byCanonical ]
  ]

FindPencil[ graph_Graph, InfraPoint[ origins_List ] ] :=
  InfraPencil[ DeleteDuplicates @ Flatten[ findPencilCore[ graph, # ] & /@ origins, 1 ] ]

FindPencil[ graph_Graph, origin_ ] :=
  InfraPencil[ findPencilCore[ graph, origin ] ]


(* ===================== PencilDirections / PencilCardinality / LineCount ===================== *)

(* PencilDirections lists the canonical lines through origin (canonical
   representatives of the constituent InfraRays); PencilCardinality is
   the pencil's size; LineCount is the projective-incidence "number of
   lines" in the graph. *)

PencilDirections[ graph_Graph, origin_ ] :=
  canonicalLine[ #[ "First" ] ] & /@ FindPencil[ graph, origin ][ "Realizations" ]

PencilCardinality[ graph_Graph, origin_ ] := FindPencil[ graph, origin ][ "Length" ]

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]
