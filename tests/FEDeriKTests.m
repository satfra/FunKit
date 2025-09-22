tests = {};

AppendTo[
    tests
    ,
    TestCreate[
        FunKit`WetterichEquation
        ,
        If[$Notebooks,
                NotebookDirectory[]
                ,
                Directory[]
            ] <> "/flows"
        ,
        TestID -> "Test flowDir default value"
    ]
];
