BeginTestSection["InfraString"]

(* ===== Auto-flatten on nested wrappers ===== *)

VerificationTest[
  InfraString[{InfraString[{{1, 2, 3}}], InfraString[{{1, 4, 1}}]}],
  InfraString[{{1, 2, 3}, {1, 4}}],
  TestID -> "InfraString-auto-flatten-nested"
]

(* ===== Canonicalisation collapses cyclic rotations ===== *)

VerificationTest[
  InfraString[{{1, 2, 3, 1}, {2, 3, 1, 2}, {3, 1, 2, 3}}],
  InfraString[{{1, 2, 3}}],
  TestID -> "InfraString-canonicalise-rotations"
]

(* ===== Orientation is preserved (no reversal quotient) ===== *)

VerificationTest[
  InfraString[{{1, 2, 3, 1}, {1, 3, 2, 1}}],
  InfraString[{{1, 2, 3}, {1, 3, 2}}],
  TestID -> "InfraString-orientation-preserved"
]

(* ===== Closed-walk input is reduced to open canonical form ===== *)

VerificationTest[
  InfraString[{{2, 3, 1, 2}}],
  InfraString[{{1, 2, 3}}],
  TestID -> "InfraString-closed-input-canonicalised-to-open"
]

(* ===== Open-walk input is closed and canonicalised ===== *)

VerificationTest[
  InfraString[{{2, 3, 1}}],
  InfraString[{{1, 2, 3}}],
  TestID -> "InfraString-open-input-closed-and-canonicalised"
]

(* ===== Repeated-vertex loops ===== *)

VerificationTest[
  InfraString[{{1, 2, 1, 3, 1}}],
  InfraString[{{1, 2, 1, 3}}],
  TestID -> "InfraString-repeated-vertex"
]

(* ===== Single-vertex constant ===== *)

VerificationTest[
  InfraString[{{1}}],
  InfraString[{{1}}],
  TestID -> "InfraString-singleton-canonical"
]

EndTestSection[]
