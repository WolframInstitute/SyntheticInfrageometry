Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[lineStructureWeights]
PackageScope[canonicalLineSeq]
PackageScope[maximalLines]


(* ===================== InfraLineStructure wrapper ===================== *)

(* InfraLineStructure[{line1, ..., linek}] is a consistent geodesic path system,
   stored as its maximal lines (longest geodesics of the system).  Unlike the
   multi-realisation wrappers, the stored list IS the lines: a structure is a
   single object, and its "realisations" are its lines. *)

InfraLineStructure[ lines_List ][ "Lines" ]        := lines
InfraLineStructure[ lines_List ][ "Realizations" ] := lines
InfraLineStructure[ lines_List ][ "First" ]        := First @ lines

(* "Length" = list of edge counts, one per line: |line| - 1. *)
InfraLineStructure[ lines_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ lines

InfraLineStructure /: Part[ InfraLineStructure[ lines_List ], i_Integer ] := lines[[ i ]]


(* ===================== FindLineStructure ===================== *)

(* A consistent geodesic path system: one shortest path P(u,v) per pair,
   subpath-closed (any stretch of a chosen path is the chosen path between its
   endpoints).  Consistency is created by a single generically-independent edge
   weighting -- the unique min-weight path per pair is automatically consistent.
   The returned InfraLineStructure stores the maximal lines of that system. *)

Options[ FindLineStructure ] = {
  Method        -> "Lexicographic",
  "Coordinates" -> False
};

FindLineStructure[ graph_Graph, opts : OptionsPattern[] ] :=
  With[
    { wg = Graph[ graph, EdgeWeight -> lineStructureWeights[ graph, OptionValue[ Method ] ] ] },
    { spf = FindShortestPath[ wg, All, All ] },
    { paths = DeleteCases[
        Association @ Map[ Sort[ # ] -> spf @@ # &, Subsets[ VertexList[ graph ], { 2 } ] ],
        { } ] },
    InfraLineStructure @ maximalLines @ Values @ paths
  ]


(* Exact 1 + 2^(-i) weighting: edges ranked 1..|E| by sorted endpoint labels.
   Distinct-subset-sum of {2^(-i)} => unique min-weight path per pair; total
   perturbation < 1 => hop-count dominates, so chosen paths are true geodesics.
   Exact Rationals only -- machine reals underflow near |E| ~ 50 and re-collide. *)

lineStructureWeights[ graph_Graph, "Lexicographic" ] :=
  With[
    { edges = EdgeList[ graph ] },
    { rank = Association @ MapIndexed[ #1 -> First[ #2 ] &, SortBy[ edges, Sort @* Apply[ List ] ] ] },
    ( 1 + 2^( -rank[ # ] ) ) & /@ edges
  ]


(* canonical orientation of a vertex sequence (lex-least of it and its reverse) *)
canonicalLineSeq[ p_List ] := First @ Sort @ { p, Reverse @ p }

(* maximal lines: chosen paths that are not a contiguous infix (either
   orientation) of a strictly longer chosen path *)
maximalLines[ paths_List ] :=
  With[ { cands = DeleteDuplicates[ canonicalLineSeq /@ paths ] },
    Select[ cands,
      c |-> NoneTrue[ cands,
        o |-> o =!= c &&
          ( SequencePosition[ o, c, 1 ] =!= { } || SequencePosition[ Reverse @ o, c, 1 ] =!= { } ) ] ]
  ]
