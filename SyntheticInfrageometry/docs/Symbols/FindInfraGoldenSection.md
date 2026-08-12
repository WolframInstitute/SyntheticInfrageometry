---
Template: Symbol
Name: FindInfraGoldenSection
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraGoldenSection
---

## Usage

`FindInfraGoldenSection[graph, p1, p2]` returns one InfraPoint at the golden index 1 + (n-1)/GoldenRatio per geodesic, unioned.

`FindInfraGoldenSection[graph, InfraSegment[{walks}]`] and FindInfraGoldenSection[graph, walk] use the supplied walks.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Method` | "Metric" (default), "Embedding" |
| `"Tolerance"` | -- |
