This is currently work in progress. More information coming soon...

To compile:
```bash
sbt test:compile
```

To run tests:
```bash
sbt test
```

To run a specific test, `CamTest` for example:
```bash
sbt testOnly verif.CamTest
```

With bloop:
```bash
bloop test verif-test -o verif.NoChiselRandomTest -- -z "Deterministic Testing"
```

## Constrained Random

- Run the End2EndSMTSpec in FIRRTL which demonstrates simple SMT generation from FIRRTL and a few lines of SMT for BMC
    `sbt:firrtl> testOnly firrtl.backends.experimental.smt.end2end.EndToEndSMTSpec`
    - Results in test_run_dir
- See the SMTCompilationTest in FIRRTL for how to compile a FIRRTL file to SMT
