---
Template: Symbol
Name: FindInfraEllipticShell
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraEllipticShell
---

## Usage

`FindInfraEllipticShell[graph, {p1, p2}, c]` returns {InfraEllipticShell[{levelSet}]} for the elliptic shell { v : d(p1,v) + d(p2,v) == c } (c may be {cMin, cMax}).

`FindInfraEllipticShell[graph, {p1, p2}, c, n]` returns exactly n or $Failed; UpTo[n] returns up to n; All returns all. Under the default Properties -> {} the level set is the one realisation; under {"Separating"} the count-less call is one minimal separating subset.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Properties` | "Separating", "Connected" |
| `Method` | Automatic (default), "Exhaustive", {"Exhaustive", "Pruning" -> spec}, "Greedy", "RandomGreedy" — Automatic resolves by the count, All to "Exhaustive" and a bounded or absent count to "Greedy"; the class is the same under every value, "RandomGreedy" peels in random order under an ambient SeedRandom, and "Pruning" caps the removable vertices tried per layer |
