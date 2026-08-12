---
Template: Symbol
Name: InfraMeasure
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraMeasure
---

## Usage

`InfraMeasure[obj]` returns the vertex occupation measure <|v -> appearances|> of an Infra* wrapper -- the marginal of its realization bundle onto its vertices.

`InfraMeasure[graph, obj]` is graph-aware and unlocks the edge measure.

## Details & Options

Options:

| Option | Values |
|---|---|
| `"On"` | "Vertices" (default), "Edges", "Both" -- and Method ("Occupation" (default) divides by numReps, the membership measure in [0,1]; "Probability" divides by total appearances, the distribution summing to 1) |
