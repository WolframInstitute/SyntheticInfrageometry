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

(* "Paths" = the unfolded association <|{u,v} -> P(u,v)|> -- every contiguous
   stretch of every line, keyed by its sorted endpoint pair, oriented small->large.
   Consistency makes the value well-defined when a pair lies on several lines. *)
InfraLineStructure[ lines_List ][ "Paths" ] :=
  Association @ Flatten @ Table[
    With[ { line = lines[[ k ]] },
      Table[
        With[ { seg = line[[ i ;; j ]] },
          Sort @ { line[[ i ]], line[[ j ]] } ->
            If[ OrderedQ @ { line[[ i ]], line[[ j ]] }, seg, Reverse @ seg ] ],
        { i, Length @ line - 1 }, { j, i + 1, Length @ line } ] ],
    { k, Length @ lines } ]

(* "Incidence" = the transpose of the line list: <|v -> {line numbers through v}|>.
   Order along each line is held by the stored sequence, so positions are not kept. *)
InfraLineStructure[ lines_List ][ "Incidence" ] :=
  Sort /@ KeySort @ GroupBy[
    Catenate @ MapIndexed[ { line, idx } |-> ( ( # -> First @ idx ) & /@ line ), lines ],
    First -> Last ]

(* "Path", u, v = recover P(u,v): the stretch of any line through both u and v,
   oriented u->v.  Well-defined by consistency; order comes from the stored line. *)
InfraLineStructure[ lines_List ][ "Path", u_, v_ ] :=
  With[ { line = SelectFirst[ lines, ContainsAll[ #, { u, v } ] & ] },
    { i = First @ FirstPosition[ line, u ], j = First @ FirstPosition[ line, v ] },
    If[ i <= j, line[[ i ;; j ]], Reverse @ line[[ j ;; i ]] ] ]

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


(* ===================== ConsistentPathSystemQ ===================== *)

(* a path system is consistent (subpath-closed) iff every contiguous stretch of a
   chosen path is itself the chosen path between its endpoints -- Cizma-Linial
   consistency = Bellman's principle of optimality.  Accepts the unfolded
   association <|{u,v} -> path|>, a maximal-line set, or an InfraLineStructure. *)

ConsistentPathSystemQ[ graph_Graph, ls_InfraLineStructure ] :=
  ConsistentPathSystemQ[ graph, ls[ "Lines" ] ]

(* line set: consistent iff no two lines induce conflicting stretches between a
   shared endpoint pair (a conflict-free unfolding is automatically subpath-closed) *)
ConsistentPathSystemQ[ graph_Graph, lines : { __List } ] :=
  AllTrue[
    GatherBy[
      Catenate @ Map[
        line |-> Catenate @ Table[
          Sort @ { line[[ i ]], line[[ j ]] } -> canonicalLineSeq @ line[[ i ;; j ]],
          { i, Length @ line - 1 }, { j, i + 1, Length @ line } ],
        lines ],
      First ],
    grp |-> SameQ @@ Values @ grp ]

(* association: each chosen path's every stretch equals the chosen path for its
   own endpoint pair (which must itself be chosen) *)
ConsistentPathSystemQ[ graph_Graph, paths_Association ] :=
  AllTrue[ Keys @ paths,
    key |-> With[ { p = paths[ key ] },
      AllTrue[ Subsets[ Range @ Length @ p, { 2 } ],
        ij |-> With[ { subkey = Sort @ { p[[ First @ ij ]], p[[ Last @ ij ]] } },
          KeyExistsQ[ paths, subkey ] &&
            canonicalLineSeq[ p[[ First @ ij ;; Last @ ij ]] ] === canonicalLineSeq @ paths[ subkey ] ] ] ] ]


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
