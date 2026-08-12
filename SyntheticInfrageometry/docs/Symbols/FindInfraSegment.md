---
Template: Symbol
Name: FindInfraSegment
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraSegment
---

## Usage

`FindInfraSegment[graph, p1, p2]` returns the compact canonical form: InfraSegment[dag] (the geodesic interval DAG of all shortest paths), or a weighted bundle of per-pair DAG atoms for weighted / multi InfraPoint endpoints.

`FindInfraSegment[graph, p1, p2, n]` returns one InfraSegment with exactly n enumerated paths or $Failed; UpTo[n] up to n; All the fully enumerated bundle.

## Details & Options

Options Properties ({} (default); {"EdgeMin", f}, {"EdgeMax", f}, "LongestPath"), Method ("Exhaustive" (default), "Greedy").
