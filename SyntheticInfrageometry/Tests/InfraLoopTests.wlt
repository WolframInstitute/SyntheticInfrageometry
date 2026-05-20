BeginTestSection["InfraLoop"]

(* ===== Auto-flatten on nested wrappers ===== *)

VerificationTest[
  InfraLoop[{InfraLoop[{{1, 2, 1}}], InfraLoop[{{1, 3, 1}}]}],
  InfraLoop[{{1, 2, 1}, {1, 3, 1}}],
  TestID -> "InfraLoop-auto-flatten-nested"
]

(* ===== Auto-close open-walk realisations ===== *)

VerificationTest[
  InfraLoop[{{1, 2, 3}}],
  InfraLoop[{{1, 2, 3, 1}}],
  TestID -> "InfraLoop-auto-close-open-walk"
]

VerificationTest[
  InfraLoop[{{1, 2, 3, 1}, {1, 4, 1}}],
  InfraLoop[{{1, 2, 3, 1}, {1, 4, 1}}],
  TestID -> "InfraLoop-already-closed-unchanged"
]

VerificationTest[
  InfraLoop[{{1, 2, 3, 1}, {1, 4, 5}}],
  InfraLoop[{{1, 2, 3, 1}, {1, 4, 5, 1}}],
  TestID -> "InfraLoop-mixed-closes-only-open"
]

EndTestSection[]
