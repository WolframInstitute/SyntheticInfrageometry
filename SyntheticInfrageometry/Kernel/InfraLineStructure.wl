Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[edgeRanking]
PackageScope[canonicalLineSeq]


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

(* "Coordinates" = incidence with positions: <|v -> {{line number, offset}, ...}|>,
   offset = number of edges from the line's start to v.  The g-free coordinate atlas;
   redundant given the stored ordered lines, so always recoverable, never stored. *)
InfraLineStructure[ lines_List ][ "Coordinates" ] :=
  Sort /@ KeySort @ GroupBy[
    Catenate @ MapIndexed[
      { line, idx } |-> MapIndexed[ ( #1 -> { First @ idx, First @ #2 - 1 } ) &, line ], lines ],
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

Options[ FindLineStructure ] = { Method -> "Lexicographic" };

(* Every Method reduces to an edge RANKING e_1, ..., e_|E|, then the exact weight
   w(e_i) = 1 + 2^(-i).  Distinct-subset-sum of {2^(-i)} => unique min-weight path
   per pair (consistent); total perturbation < 1 => hop-count dominates (chosen
   paths are true geodesics).  So any ranking yields a consistent geodesic system;
   the Method only changes which geodesic wins among hop-count ties.  Exact
   Rationals -- machine reals underflow near |E| ~ 50 and re-collide. *)

FindLineStructure[ graph_Graph, opts : OptionsPattern[] ] :=
  With[
    { rank = Association @ MapIndexed[ #1 -> First[ #2 ] &,
        edgeRanking[ graph, EdgeList @ graph, OptionValue[ Method ] ] ] },
    { wg = Graph[ graph, EdgeWeight -> ( ( 1 + 2^( -rank[ # ] ) ) & /@ EdgeList @ graph ) ] },
    { spf = FindShortestPath[ wg, All, All ] },
    { paths = DeleteCases[
        Association @ Map[ Sort[ # ] -> spf @@ # &, Subsets[ VertexList[ graph ], { 2 } ] ],
        { } ] },
    { cands = DeleteDuplicates[ canonicalLineSeq /@ Values @ paths ] },
    (* maximal lines: chosen paths that are not a contiguous infix (either
       orientation) of a strictly longer chosen path *)
    InfraLineStructure @ Select[ cands,
      c |-> NoneTrue[ cands,
        o |-> o =!= c &&
          ( SequencePosition[ o, c, 1 ] =!= { } || SequencePosition[ Reverse @ o, c, 1 ] =!= { } ) ] ]
  ]


(* the edges in ranked order; the ranking key is the only thing a Method changes *)
edgeRanking[ graph_, edges_, "Lexicographic" ] := SortBy[ edges, Sort @* Apply[ List ] ]

edgeRanking[ graph_, edges_, { "Random", seed_ } ] :=
  BlockRandom[ SeedRandom[ seed ]; RandomSample @ edges ]

(* resistance is only a ranking key, so machine PseudoInverse is fine and self-
   contained; the 2^(-i) weights supply genericity, sorted endpoints break ties *)
edgeRanking[ graph_, edges_, "Resistance" ] :=
  With[
    { lp = PseudoInverse @ N @ KirchhoffMatrix @ graph, idx = First /@ PositionIndex @ VertexList @ graph },
    SortBy[ edges,
      e |-> { With[ { a = idx[ First @ e ], b = idx[ Last @ e ] }, lp[[ a, a ]] + lp[[ b, b ]] - 2 lp[[ a, b ]] ],
              Sort @ Apply[ List, e ] } ]
  ]

(* user edge function w[edge] as the ranking key; sorted endpoints break w-ties, so
   no collision detection is needed -- the ranking is always a total order *)
edgeRanking[ graph_, edges_, ( "Weight" -> w_ ) ] :=
  SortBy[ edges, e |-> { w[ e ], Sort @ Apply[ List, e ] } ]


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

