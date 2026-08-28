Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraEffectivePoint wrapper ===================== *)

(* A effective point: a finitely supported measure on the vertex set -- the measure
   layer of the point ontology (InfraPoint atom / InfraSet family /
   InfraEffectivePoint measure).  Constructed at projections (seg[[i]], midpoint
   bands, shell-center multiplicity) and by the canonical up-coercions below.
   The all-ones measure stays a effective point -- layers never cross silently.
   Bare vertex lists are NOT accepted (a list-labelled vertex is ambiguous
   against a support list); wrap atoms or pass the association. *)

InfraEffectivePoint[ inner_InfraEffectivePoint ] := inner

(* input sugar: parallel support / mass lists; duplicate vertices sum *)
InfraEffectivePoint[ verts_List, masses_List ] /; AllTrue[ masses, NumericQ ] :=
  InfraEffectivePoint[ Merge[ Thread[ verts -> masses ], Total ] ]

(* up-coercion: a list of points is the counting measure -- repetition is mass *)
InfraEffectivePoint[ points : { __InfraPoint } ] :=
  InfraEffectivePoint @ Merge[ infraVertexMultiset /@ points, Total ]

(* up-coercion: any other Infra* object contributes its vertex marginal --
   InfraSet the all-ones measure, a bundle its occupation counts (a
   geodesic-DAG atom by the Brandes DP, no enumeration).  Runtime head test as
   in InfraPoint.wl: $infraBundleHeads need not be assigned when this file is
   read. *)
InfraEffectivePoint[ obj_ ] /;
    Length[ obj ] === 1 && MatchQ[ Head @ obj, $infraBundleHeads | InfraPoint | InfraObject | InfraSet ] :=
  InfraEffectivePoint @ infraVertexMultiset @ obj

(* canonical form: keys sorted, so two equal measures built by different routes
   compare SameQ -- the DAG form and the enumerated form of the same segment
   used to differ by key order alone.  Guarded against PATTERN arguments for the
   same reason as InfraSet: DownValues evaluate patterns, so without the FreeQ a
   rule-author's InfraEffectivePoint[<|v_ -> m_|>] would be rewritten. *)
InfraEffectivePoint[ m_Association ] /;
    FreeQ[ m, _Blank | _BlankSequence | _BlankNullSequence | _Pattern ] &&
    Keys[ m ] =!= Sort @ Keys[ m ] :=
  InfraEffectivePoint @ KeySort @ m


InfraEffectivePoint[ m_Association ][ "Support" ]  := InfraSet[ Sort @ Keys @ m ]
InfraEffectivePoint[ m_Association ][ "Vertices" ] := Keys @ m
InfraEffectivePoint[ m_Association ][ "Weights" ]  := Values @ m
InfraEffectivePoint[ m_Association ][ "Mass" ]     := Total @ m
InfraEffectivePoint[ m_Association ][ "First" ]    := First @ Keys @ m

(* Shannon entropy of the normalised measure -- 0 iff the effective point is sharp *)
InfraEffectivePoint[ m_Association ][ "Entropy" ]  :=
  With[ { p = Values[ m ] / Total[ m ] }, -Total[ p * Log[ p ] ] ]

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw masses; ["Measure"] = m/N; ["ProbabilityMeasure"] = m/Total. *)
InfraEffectivePoint[ m_Association ][ "OccupationCount" ]    := m
InfraEffectivePoint[ m_Association ][ "OccupationMeasure" ]  := InfraMeasure[ InfraEffectivePoint[ m ] ]
InfraEffectivePoint[ m_Association ][ "Measure" ]            := InfraMeasure[ InfraEffectivePoint[ m ] ]
InfraEffectivePoint[ m_Association ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraEffectivePoint[ m ], Method -> "Probability" ]
