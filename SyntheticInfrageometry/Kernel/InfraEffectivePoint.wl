Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraEffectivePoint wrapper ===================== *)


InfraEffectivePoint[ inner_InfraEffectivePoint ] := inner

InfraEffectivePoint[ verts_List, masses_List ] /; AllTrue[ masses, NumericQ ] :=
  InfraEffectivePoint[ Merge[ Thread[ verts -> masses ], Total ] ]

InfraEffectivePoint[ points : { __InfraPoint } ] :=
  InfraEffectivePoint @ Merge[ infraVertexMultiset /@ points, Total ]

(* runtime head test as in InfraPoint.wl: $infraBundleHeads need not be assigned when this file is read *)
InfraEffectivePoint[ obj_ ] /;
    Length[ obj ] === 1 && MatchQ[ Head @ obj, $infraBundleHeads | InfraPoint | InfraObject | InfraSet ] :=
  InfraEffectivePoint @ infraVertexMultiset @ obj

(* keys sorted, so equal measures built by different routes compare SameQ; the FreeQ guard keeps DownValues from rewriting a rule-author's pattern *)
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

InfraEffectivePoint[ m_Association ][ "OccupationCount" ]    := m
InfraEffectivePoint[ m_Association ][ "OccupationMeasure" ]  := InfraMeasure[ InfraEffectivePoint[ m ] ]
InfraEffectivePoint[ m_Association ][ "Measure" ]            := InfraMeasure[ InfraEffectivePoint[ m ] ]
InfraEffectivePoint[ m_Association ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraEffectivePoint[ m ], Method -> "Probability" ]
