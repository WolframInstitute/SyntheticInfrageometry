#!/usr/bin/env wolframscript

$pacletDir = DirectoryName[$InputFileName];

PacletDirectoryLoad[$pacletDir <> "/SyntheticInfrageometry"];
Needs["WolframInstitute`SyntheticInfrageometry`"];

$testFiles = FileNames["*.wlt", FileNameJoin[{$pacletDir, "SyntheticInfrageometry", "Tests"}]];

Print["Running ", Length[$testFiles], " test files..."];
Print[""];

$totalPassed = 0;
$totalFailed = 0;
$totalErrored = 0;
$totalTests = 0;

Scan[
    Function[testFile,
        Print["Running: ", FileNameTake[testFile]];
        With[{report = TestReport[testFile]},
            With[{
                passed = Length[report["TestsSucceededIndices"]],
                failed = Length[report["TestsFailedIndices"]],
                errored = Length[report["TestsFailedWithErrorsIndices"]],
                total = Length[report["TestResults"]]
            },
                $totalPassed += passed;
                $totalFailed += failed;
                $totalErrored += errored;
                $totalTests += total;
                Print["  ", passed, "/", total, " passed"];
                If[failed > 0 || errored > 0,
                    Scan[
                        Function[idx,
                            With[{tr = report["TestResults"][idx]},
                                Print["  FAIL: ", tr["TestID"], " -- ", tr["Outcome"]];
                            ]
                        ],
                        Join[report["TestsFailedIndices"], report["TestsFailedWithErrorsIndices"]]
                    ];
                ];
            ]
        ];
        Print[""];
    ],
    $testFiles
];

Print["===== Test Summary ====="];
Print["Total:   ", $totalTests];
Print["Passed:  ", $totalPassed];
Print["Failed:  ", $totalFailed];
Print["Errors:  ", $totalErrored];
Print[""];

If[$totalFailed > 0 || $totalErrored > 0,
    Print["Some tests failed."];
    Exit[1],
    Print["All tests passed!"];
    Exit[0]
]
