---
Template: Symbol
Name: FindInfraPolygon
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPolygon
---

## Usage

`FindInfraPolygon[graph, {p1, ..., pn}]` returns {InfraPolygon[{poly}]} for the polygon through corners p1, ..., pn (n >= 3), each side a geodesic between consecutive corners and the last side returning to p1.

`FindInfraPolygon[graph, verts, count]` returns exactly count or $Failed; UpTo[count] returns up to count; All returns all polygons (the Cartesian product of per-side geodesic realisations).

## Details & Options

Corners accept bare vertices or InfraPoint atoms.

Default count = 1 (first geodesic per side).

Options:

| Option | Values |
|---|---|
| `Properties / Method forward to the side geodesics` | see FindInfraSegment |
