---
Template: Symbol
Name: FindInfraHomotopyRepresentative
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraHomotopyRepresentative
---

## Usage

`FindInfraHomotopyRepresentative[graph, obj]` returns {head[{w}]} for a length-shortest walk in obj's homotopy class (head matches the input wrapper; InfraCircle coerces to InfraString); n / UpTo[n] / All controls multiplicity.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Method` | "Exhaustive" (default), "Greedy" |
| `"FreeHomotopy"` | -- |
| `"NullHomotopicCycles"` | -- |
| `"MaxLength"` | -- |
| `"MaxMoves"` | -- |
