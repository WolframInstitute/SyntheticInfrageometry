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

`FindInfraEllipticShell[graph, {p1, p2}, c, n]` returns exactly n or $Failed; UpTo[n] returns up to n; All returns all.

## Details & Options

Options Properties ("Separating", "Connected"), Method ("Exhaustive" (default), "Greedy").
