---
Template: Symbol
Name: FindLineStructure
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindLineStructure
---

## Usage

`FindLineStructure[graph]` returns an InfraLineStructure[{lines}]: a consistent (subpath-closed) geodesic path system -- one shortest path per pair, with every stretch of a chosen path again the chosen path -- stored as its maximal lines.

## Details & Options

Option Method ("Lexicographic" (default; exact 1 + 2^(-rank) weighting, edges ranked by sorted endpoints) | {"Random", seed} | "Resistance" | "Weight" -> w) supplies the edge ranking that breaks geodesic ties; every method yields a consistent system.

See InfraLineStructure for accessors.
