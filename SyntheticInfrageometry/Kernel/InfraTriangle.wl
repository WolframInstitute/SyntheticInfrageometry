Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindInfraTriangle ===================== *)

(* the polygon on three corners: its three geodesic sides, one directed path graph each *)

FindInfraTriangle::badmethod = "Method `1` is not supported by FindInfraTriangle.";

Options[ FindInfraTriangle ] = { Method -> Automatic };

FindInfraTriangle[ graph_Graph, vertices_List /; Length[ vertices ] === 3,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  findPolygonCore[ FindInfraTriangle, graph, vertices, count, opts ]


(* ===================== InfraTriangleQ ===================== *)

InfraTriangleQ[ graph_Graph, polys : { { __Graph } .. } ] :=
  AllTrue[ polys, InfraTriangleQ[ graph, # ] & ]

InfraTriangleQ[ graph_Graph, sides : { _Graph, _Graph, _Graph } ] :=
  InfraPolygonQ[ graph, sides ]

InfraTriangleQ[ _Graph, _ ] := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraTriangle[ verts_List, opts___Rule ] ] :=
  capBranches[
    polylineToVertexSeq /@ FindInfraTriangle[ graph, verts, All,
      Sequence @@ FilterRules[ { opts }, Options[ FindInfraTriangle ] ] ],
    extractBranches[ { opts } ] ]
