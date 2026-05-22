Package["WolframInstitute`SyntheticInfrageometry`"]


(* Polymorphic equality on the diffusion diagram of Infra* wrappers.
   Two wrappers are compared via their vertex-multiset (the diffuse-rendering
   weight), with a Method axis ordered loose -> strict:

     "Overlap"   |A cap B| > 0                  (one common vertex)
     "Diffuse"   |A cap B| > |A delta B|         (weighted Jaccard > 1/2; default)
     "Set"       Sort@Keys[A]   == Sort@Keys[B]   (vertex sets identical)
     "Multiset"  KeySort[A]     == KeySort[B]    (diffusion diagrams identical)

   Heads must match; cross-head comparison returns False. *)

InfraEqualQ::badmethod = "Method `1` is not one of \"Overlap\", \"Diffuse\", \"Set\", \"Multiset\"."

Options[ InfraEqualQ ] = { Method -> "Diffuse" }

InfraEqualQ[ _Graph, a_, b_, OptionsPattern[] ] /; Head[ a ] =!= Head[ b ] := False

InfraEqualQ[ _Graph, a_, b_, OptionsPattern[] ] :=
  With[ { ma = infraVertexMultiset @ a, mb = infraVertexMultiset @ b },
    Switch[ OptionValue @ Method,
      "Overlap",  multisetCapMass[ ma, mb ] > 0,
      "Diffuse",  3 multisetCapMass[ ma, mb ] > Total @ ma + Total @ mb,
      "Set",      Sort @ Keys @ ma === Sort @ Keys @ mb,
      "Multiset", KeySort @ ma === KeySort @ mb,
      m_,         Message[ InfraEqualQ::badmethod, m ]; $Failed
    ]
  ]

multisetCapMass[ a_Association, b_Association ] :=
  Total @ KeyValueMap[ { k, v } |-> Min[ v, Lookup[ b, k, 0 ] ], a ]
