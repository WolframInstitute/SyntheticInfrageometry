Get["SyntheticInfrageometry/Tools.wl"]
Get["SyntheticInfrageometry/Postulates.wl"]
Get["SyntheticInfrageometry/Predicates.wl"]
Get["SyntheticInfrageometry/Constructions.wl"]
Get["SyntheticInfrageometry/InfrageometricScene.wl"]

Print["Running ToolsTests..."]
Print[TestReport["Tests/ToolsTests.wlt"]]

Print["Running PostulatesTests..."]
Print[TestReport["Tests/PostulatesTests.wlt"]]

Print["Running PredicatesTests..."]
Print[TestReport["Tests/PredicatesTests.wlt"]]

Print["Running ConstructionsTests..."]
Print[TestReport["Tests/ConstructionsTests.wlt"]]

Print["Running InfrageometricSceneTests..."]
Print[TestReport["Tests/InfrageometricSceneTests.wlt"]]
