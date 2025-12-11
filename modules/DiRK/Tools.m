FSetSymmetricDressing[obj_, {f__}] :=
    Module[{},
        Unprotect[dressing];
        dressing[obj, {f}, n_, {any__}] /; Not @ OrderedQ[{any}] := dressing[obj, {f}, n, Sort @ {any}];
        Protect[dressing];
    ];

FSetSymmetricDressing[obj_, {f__}, {i__Integer}] :=
    Module[{},
        Unprotect[dressing];
        dressing[obj, {f}, n_, {any__}] /; Not @ OrderedQ[{any}[[{i}]]] :=
            Module[{new = {any}},
                new[[{i}]] = Sort @ new[[{i}]];
                dressing[obj, {f}, n, new]
            ];
        Protect[dressing];
    ];

FSetSymmetricDressing[obj_, {f__}, n_Integer] :=
    Module[{},
        Unprotect[dressing];
        dressing[obj, {f}, n, {any__}] /; Not @ OrderedQ[{any}] := dressing[obj, {f}, n, Sort @ {any}];
        Protect[dressing];
    ];

FSetSymmetricDressing[obj_, {f__}, n_Integer, {i__Integer}] :=
    Module[{},
        Unprotect[dressing];
        dressing[obj, {f}, n, {any__}] /; Not @ OrderedQ[{any}[[{i}]]] :=
            Module[{new = {any}},
                new[[{i}]] = Sort @ new[[{i}]];
                dressing[obj, {f}, n, new]
            ];
        Protect[dressing];
    ];
