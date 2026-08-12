---
Template: Symbol
Name: FindInfraTriangle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraTriangle
---

## Usage

`FindInfraTriangle[graph, {a, b, c}]` returns {InfraTriangle[{poly}]} for the triangle through corners a, b, c (the n = 3 case of FindInfraPolygon); each side a geodesic.

`FindInfraTriangle[graph, verts, count]` / UpTo[count] / All controls multiplicity over the Cartesian product of per-side geodesics; default count = 1.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Properties / Method forward to the side geodesics` | see FindInfraSegment |
