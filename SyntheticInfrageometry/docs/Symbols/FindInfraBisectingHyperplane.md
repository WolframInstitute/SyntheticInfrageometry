---
Template: Symbol
Name: FindInfraBisectingHyperplane
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraBisectingHyperplane
---

## Usage

`FindInfraBisectingHyperplane[graph, p1, p2]` returns {InfraPlane[{slab}]} for the bisector { v : d(p1, v) == d(p2, v) }; a positional {lo, hi} widens the slab to lo <= d(p1, v) - d(p2, v) <= hi.

`FindInfraBisectingHyperplane[graph, p1, p2, n]` returns exactly n or $Failed; UpTo[n] returns up to n; All returns all.

## Details & Options

Options Properties ("Separating", "Connected"), Method ("Exhaustive" (default), "Greedy").
