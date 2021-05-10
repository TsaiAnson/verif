# Co-Simulation Framework for Chisel (CFC)

## Configuration
### Initial Setup
Enabling co-simulation within Chipyard requires a custom version of `spike`, a compatible commit of `gemmini`, and a 
local clone of `chiseltest`. In order to simplify the setup process and ensure compatibility, there is a `cosim` branch 
in Chipyard that contains the necessary libraries and pointers. Using this branch, setting up co-simulation is as simple
as following the steps below.

```bash
~/> git clone git@github.com:ryan-lund/chipyard
~/> cd chipyard
~/chipyard> git checkout cosim
~/chipyard> ./scripts/init-submodules-no-riscv-tools.sh
~/chipyard> ./scripts/build-toolchains.sh esp-tools
~/chipyard> source env.sh
~/chipyard> source tools/verif/cosim/cosim_env.sh
~/chipyard> ./tools/verif/cosim/setup.sh
```

Within this block of instructions, `cosim_env.sh` checks if `protoc --verion >= 3.3.0`. If it is not, the script will
install a local version of `protoc` in `cosim/install`.

`setup.sh` uses `protoc` to build the protocol buffer files in `resources/proto`, copies those files to relevant
locations, and builds `spike`.

Note that on some machines with older `make` versions (particularly the Berkeley eda machines) `./scripts/build-toolchains.sh esp-tools` may need 
to be replaced with `scl enable devtoolset-8 './scripts/build-toolchains.sh esp-tools'`.

### Subsequent Shells
Once co-simulation has been set up, the only action that need to be taken by subsequent shells is to source
`cosim_env.sh`. For frequent co-simulation use, it is recommended that you add this step to your `bashrc`.

```bash
~/> source cosim_env.sh
```

## Compiling/Running Tests
Co-simulation compilation and tests must be run from within `sbt`. Because co-simulation depends on Chipyard, `sbt` 
should be launched for Chipyard as a whole rather than the `verif` sub-project.

```bash
~/chipyard> cd sims/verilator
~/chipyard> make launch-sbt
sbt:chipyard> project verifCosim
```

To compile src (files within ./src/main/scala):
```bash
sbt:verifCosim> compile
```
To compile tests (files within ./src/test/scala):
```bash
sbt:verifCosim> test:compile
```

Once tests are compiled, a target Gemmini configuration can be elaborated and used to re-build the tests within
`gemmini-rocc-tests` by executing the following command.

```bash
sbt:verifCosim> testOnly cosim.GemminiTest -- -z "build"
```

Finally, a specific compiled binary (addressed relative to the Chipyard root), can be run using the following test, 
where `<target>` is replaced with a target test binary.

```bash
sbt:verifCosim> testOnly cosim.GemminiTest -- -z "target" -DsimTarget=<target>
```

For example, to the `mvin_mvout-baremetal` test:

```bash
sbt:verifCosim> testOnly cosim.GemminiTest -- -z "target" -DsimTarget=\generators\gemmini\software\gemmini-rocc-tests\build\bareMetalC\mvin_mvout-baremetal
```

## Modifying the Gemmini Configuration
Modifying the Gemmini configuration used in co-simulation is as simple as modifying the configuration used in the call 
to `new Gemmini(...)` in the following lines of [`GemminiTest.scala`](test/GemminiTest.scala).

```bash
val dut = LazyModule(
    new VerifRoCCStandaloneWrapper(
      /*** SET YOUR CONFIGURATION FOR COSIM HERE ***/
      () => new Gemmini(GemminiConfigs.defaultConfig.copy(use_dedicated_tl_port = true,
    meshRows = 4, meshColumns = 4, rob_full_entries = 4)),
    beatBytes = 16,
    addSinks = 1
  ))
```

Whenever modifications are made, the build test target should be executed again to ensure that the tests within 
`gemmini-rocc-tests` are updated to match the current Gemmini configuration.

```bash
sbt:verifCosim> testOnly cosim.GemminiTest -- -z "build"
```

## Additional Resources
Additional information on the co-simulation framework and an analysis of speed-ups resulting from it use can be found in 
[this Master's thesis](TODO).