---
Template: Symbol
Name: FindInfraWalk
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraWalk
---

## Usage

`FindInfraWalk[graph, p1, kspec]` grows the walks from p1 in the class cut by the Properties rules (default {"Simple"}) until a stopping condition or the budget kspec (UpTo[k], {k}, {lo, hi}, Infinity) stops them; FindInfraWalk[graph, p1, p2, kspec] keeps those ending at p2.

## Details & Options

Each walk is a path graph on position pairs.

Options "InfraScale", Properties, "StoppingCondition", Method.
