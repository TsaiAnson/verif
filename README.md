# Verification Library for Chisel RTL

A library to verify Chisel-generated RTL built on top of [Chiseltest](https://github.com/ucb-bar/chisel-testers2).
Still work in progress, APIs are not stable.

## Configuration
### Standalone
The `core` subproject only depends on Chisel 3.4+ and Chiseltest.
To run tests:
```bash
~/> git clone git@github.com:TsaiAnson/verif && cd verif
~/verif> sbt
sbt:verif> project core
sbt:core> testOnly verif.RandomTest
```

### As a Library
If you have a Chisel sbt project at `~/proj` and want to use the core library, submodule this repo
```bash
~/proj> git submodule add git@github.com:TsaiAnson/verif
```
Add these lines to `~/proj/build.sbt`:
```sbt
val directoryLayout = Seq(
  scalaSource in Compile := baseDirectory.value / "src",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Test := baseDirectory.value / "resources",
)

val verifSettings = Seq(
  scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11", "-language:reflectiveCalls"),
  libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.+" % "test",
  libraryDependencies += "edu.berkeley.cs" %% "chisel3" %% "3.4.+"
)

lazy val verif = (project in file("./verif/core"))
  .settings(directoryLayout)
  .settings(verifSettings)
```

Alternatively, you can pull this repo as a Git dependency:
```sbt
lazy val verif = ProjectRef(uri("git://github.com/TsaiAnson/verif.git"), "core")
lazy val main = Project("root", file(".")).dependsOn(verif)
```

### Inside Chipyard
If you want to use the TileLink VIPs or cosimulation capabilities, `verif` uses dependencies ([Rocket-Chip](https://github.com/chipsalliance/rocket-chip), [DSPTools](https://github.com/ucb-bar/dsptools), [Gemmini](https://github.com/ucb-bar/gemmini)) from [Chipyard](https://github.com/ucb-bar/chipyard).

Clone Chipyard and add this repo as a submodule:
```bash
~/> git clone git@github.com:ucb-bar/chipyard
~/> cd chipyard
~/chipyard> git checkout dev
~/chipyard> ./scripts/init-submodules-no-riscv-tools.sh
~/chipyard> cd tools
~/chipyard/tools> git submodule add git@github.com:TsaiAnson/verif
```

Add the following snippet to the end of `chipyard/build.sbt`:
```sbt
val directoryLayout = Seq(
  scalaSource in Compile := baseDirectory.value / "src",
  javaSource in Compile := baseDirectory.value / "src",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Test := baseDirectory.value / "resources",
)

val verifSettings = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  ),
  scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11", "-language:reflectiveCalls"),
  libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.+" % "test"
)

lazy val verifCore = (project in file("./tools/verif/core"))
  .settings(directoryLayout)
  .sourceDependency(chiselRef, chiselLib)
  .settings(commonSettings)
  .settings(verifSettings)

lazy val verifTL = (project in file("./tools/verif/tilelink"))
  .settings(directoryLayout)
  .sourceDependency(chiselRef, chiselLib)
  .dependsOn(rocketchip, chipyard, dsptools, `rocket-dsptools`, verifCore)
  .settings(commonSettings)
  .settings(verifSettings)

lazy val verifGemmini = (project in file("./tools/verif/cosim"))
  .settings(directoryLayout)
  .sourceDependency(chiselRef, chiselLib)
  .dependsOn(rocketchip, chipyard, dsptools, `rocket-dsptools`, gemmini, verifCore)
  .settings(commonSettings)
  .settings(verifSettings)
  .settings(libraryDependencies += "com.google.protobuf")
  .settings(libraryDependencies += "com.google.protobuf" % "protobuf-java-util" % "3.14.0") % "protobuf-java" % "3.11.0")
```

Run tests from Chipyard:
```bash
~/chipyard> cd sims/verilator
~/chipyard/sims/verilator> make launch-sbt
sbt:chipyardRoot> project verifTL
sbt:verifTL> testOnly verif.TLL2CacheTest
```

## Directory Structure
```
.
├── README.md
├── build.sbt
├── project/
├── core/               [verifCore]
│   ├── src/
│   │   ├── smt/        (For constrained random)
│   │   ├── maltese/
│   │   └── *.scala     (Source Files)
│   └── test/
│       ├── designs/    (Various Hardware/Software designs for Verif functionality)
│       └── *Test.scala (Test Files)
├── cosim/              [verifGemmini]
│   ├── src/
│   │   ├── resources/
│   │   └── *.scala     (Source Files)
│   └── test/
│       └── *Test.scala (Test Files)
└── tilelink/           [verifTL]
    ├── src/
    │   └── *.scala     (Source Files)
    └── test/
        └── *Test.scala (Test Files)
```

## Compiling/Running Tests
As this project uses `sbt`, you can build/compile source code and tests using either the `sbt` command or console.

To compile Verif src (files within ./src/main/scala):
```bash
sbt compile
```
To compile tests (files within ./src/test/scala):
```bash
sbt test:compile
```
To compile and run all tests:
```bash
sbt test
```
To compile and run a specific test, `CamTest` for example:
```bash
sbt testOnly verif.CamTest
```

## Writing Tests
Tests in this library follow a UVM-like structure, where a driver pushes input transactions to the DUT and a monitor reads response transactions from the DUT. While there is no scoreboard object, there is functionality that checks DUT response transactions against a golden software model's. Below is an example structure of a test that checks a Queue (DUT) against a golden model. Note that `ChiselTest` is used as the test simulator.

```scala
it should "Queue Test" in {
  test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

    // Defining Driver and Monitor Instances
    val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)
    val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)

    // Defining input transcations. See later sections for input transaction generation.
    val inputTransactions = Seq(...)
    qInAgent.push(inputTransactions)

    // Running test simulation
    val simCycles = 80
    c.clock.step(simCycles)

    // Retrieving DUT response transactions
    val output = qOutAgent.getMonitoredTransactions.toArray[DecoupledTX[UInt]]

    // Running Golden SW Model
    val model = new SWIntQueue(8)
    val swoutput = model.process(inputTransactions, simCycles, waitCycles).toArray[DecoupledTX[UInt]]

    // Checking DUT responses against the model's
    assert(outputChecker.checkOutput(output, {t : DecoupledTX[UInt] => (t.data.litValue(), t.cycleStamp)},
      swoutput, {t : DecoupledTX[UInt] => (t.data.litValue(), t.cycleStamp)}))
  }
}
```
Note that because the Queue has a decoupled interface, we use Verif's decoupled VIP. The library also contains other VIP for other interfaces as well. Currently, we have VIPs for a generic interface, decoupled (ready/valid) interface, and the TileLink interface (more coming later). Please refer to the following sections for more information on specific interfaces.

### Generic VIP
A generic interface describes a simple input-output interface with no special dependency signals (like ready-valid). The driver and monitor for this interface simply pushes and reads transactions on every cycle.  Below is an example of using the generic VIPs to test a CAM:

```scala
class CamTest extends FlatSpec with ChiselScalatestTester {
  it should "cam test" in {
    // Define the DUT inside the "test" construct
    test(new ParameterizedCAMAssociative(8,8,8))
      .withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        // Defining and connecting Driver + Monitor to DUT
        val camInAgent = new GenericDriver[CAMIO](c.clock, c.io)
        val camOutAgent = new GenericMonitor[CAMIO](c.clock, c.io)

        // Hard coding transactions for reference (see below section for randomized transactions)
        val protoTx = CAMIO(8, 8)
        val inputTransactions = Seq(
          protoTx.Lit(_.en -> false.B, _.we -> true.B, _.keyRe -> 0.U, _.keyWr -> 10.U, _.dataWr -> 123.U, _.found -> false.B, _.dataRe -> 0.U),
          protoTx.Lit(_.en -> true.B, _.we -> false.B, _.keyRe -> 10.U, _.keyWr -> 0.U, _.dataWr -> 0.U, _.found -> false.B, _.dataRe -> 0.U)
        )

        camInAgent.push(inputTransactions)
        c.clock.step(inputTransactions.length + 1)
        val output = camOutAgent.getMonitoredTransactions.toArray[CAMIO]

        val model = new SWAssocCAM(8,8,8)
        val swoutput = inputTransactions.map(inpTx => model.process(inpTx)).toArray[CAMIO]

        assert(outputChecker.checkOutput(output.slice(1,output.size), {t : CAMIO => (t.found.litToBoolean, t.dataRe.litValue())},
        swoutput, {t : CAMIO => (t.found.litToBoolean, t.dataRe.litValue())}))
    }
  }
}
```

### Decoupled VIP
A decoupled interface describes an interface that contains ready-valid signals. The driver and monitor operate on custom `DecoupledTX` transaction objects that contain metadata such as `waitCycles`(# of cycles to wait before pushing txn) and `postSendCycles`(# of cycles to wait after pushing txn) to allow for the customization transaction timing. The monitor can also be configured to wait some number of cycles to introduce artifical backpressure. Below is an example of using the decoupled VIPs to test a Queue:

```scala
class QueueTest extends FlatSpec with ChiselScalatestTester {
  it should "Queue Test" in {
  // Define the DUT inside the "test" construct
    test(new Queue(UInt(8.W), 8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>

      val qInAgent = new DecoupledDriver[UInt](c.clock, c.io.enq)
      val qOutAgent = new DecoupledMonitor[UInt](c.clock, c.io.deq)

      // Configuring monitor to wait 2 cycles between reading each transaction
      val waitCycles = 2
      qOutAgent.setConfig("waitCycles", waitCycles)

      // Hard coding transactions for reference (see below section for randomized transactions)
      val inputTransactions = Seq(
        DecoupledTX(165.U,0.U,1.U),
        DecoupledTX(122.U,1.U,1.U)
      )
      qInAgent.push(inputTransactions)

      // Must ensure that there are enough cycles for the whole test
      val simCycles = 80
      c.clock.step(simCycles)

      val output = qOutAgent.getMonitoredTransactions.toArray[DecoupledTX[UInt]]

      val model = new SWIntQueue(8)
      val swoutput = model.process(inputTransactions, simCycles, waitCycles).toArray[DecoupledTX[UInt]]

      // Cycle offset between software and DUT (Need to fix)
      val cycleOffset = 2
      assert(outputChecker.checkOutput(output, {t : DecoupledTX[UInt] => (t.data.litValue(), t.cycleStamp - cycleOffset)},
        swoutput, {t : DecoupledTX[UInt] => (t.data.litValue(), t.cycleStamp)}))
    }
  }
}
```

### TileLink VIP
TileLink (TL) is a custom interface used in RocketChip, mainly for it's internal memory hierarchy. The TL interface uses a master to slave connection system, so the TL VIP contains drivers and monitors for both interfaces. Moreover, the TL interface has a few variants, namely TL-UL (Uncached Lightweight), TL-UH (Uncached Heavyweight), and TL-C (Cached). Currently, only the TL-UL variant is supported, but support for the other variants are in development. For more information regarding the different variants, you can view the TileLink spec [here](https://sifive.cdn.prismic.io/sifive%2Fcab05224-2df1-4af8-adee-8d9cba3378cd_tilelink-spec-1.8.0.pdf).

While the TL VIP contains two interfaces, one for the master, and one for the slave, the usage of these drivers and monitors will look the same. The only difference is the functionality. The TL VIP for the slave interface will be responsible for driving input and recording responses to and from the slave component. The TL VIP for the master interface will be responsible for receiving requests and returning responses from and to the master component. The code below is an example of testing a slave component, `VerifTLRegBankSlave`:

```scala
class SWTLFuzzerTest extends FlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors

  it should "Test TLRegBank" in {
    val TLRegBankSlave = LazyModule(new VerifTLRegBankSlave with VerifTLStandaloneBlock)
    test(TLRegBankSlave.module).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      // Connecting DUT to Driver and Monitor (Note the same interface --- the "in" port also carries the slave's response)
      val passInAgent = new TLSlaveDriverBasic(c.clock, TLRegBankSlave.in)
      val passOutAgent = new TLSlaveMonitorBasic(c.clock, TLRegBankSlave.in)

      // Example of using the software TL Transaction fuzzer (currently only for TL interfaces)
      val fuz = new SWTLFuzzer(TLRegBankSlave.standaloneSlaveParams.managers(0), overrideAddr = Some(AddressSet(0x00, 0x1ff)))
      val inputTransactions = fuz.generateTransactions(60)
      passInAgent.push(inputTransactions)

      val simCycles = 150
      c.clock.step(simCycles)
      val output = passOutAgent.getMonitoredTransactions.toArray

      // Generating SW Model's output
      val model = new SWRegBank(regCount = 64, regSizeBytes = 8)
      val swoutput = model.process(inputTransactions).toArray

      // Checking DUT's output with model's
      assert(outputChecker.checkOutput(output, {t : TLTransaction => t},
        swoutput, {t : TLTransaction => t}))
    }
  }
}
```

For a code example of testing a master component, please view the test `VerifTL Test Master` within `DSPToolsTest.scala`. The test format will be similar.

#### TileLink Standalone Block
As TileLink components require a complete connection (with both master and slave) to elaborate, extra IP is required to enable the elaboration of a standalone TL component within a test. Below is the trait `VerifTLStandaloneBlock` that allows for such connections:

```scala
trait VerifTLStandaloneBlock extends LazyModule with VerifTLBase {
  val ioInNode = BundleBridgeSource(() => TLBundle(verifTLBundleParams))
  val ioOutNode = BundleBridgeSink[TLBundle]()

  val TLMaster: TLOutwardNode
  val TLSlave: TLInwardNode

  ioOutNode :=
    TLToBundleBridge(standaloneSlaveParams) := TLMaster

  TLSlave :=
    BundleBridgeToTL(standaloneMasterParams) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}
```

The function of the above block is to connect to the TL component (either the master and/or slave) to allow elaboration and to expose the TL interface for testing. To use, simply use the trait as a mixin when instantiating your TL component (see above test example), and ensure that both `TLMaster` and `TLSlave` is defined. For example, the `VerifTLRegBankSlave` is shown below:

```scala
class VerifTLRegBankSlave(implicit p: Parameters) extends LazyModule  {
  // Defining TLSlave as a Register Node
  val TLSlave = TLRegisterNode(
    address = Seq(AddressSet(0x0, 0xfff)),
    device = new SimpleDevice("VerifTLRegBankSlave", Seq("")),
    beatBytes = 8,
    concurrency = 1)

  // Filler for now
  val TLMaster = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "testmaster",
    sourceId = IdRange(0,1),
    requestFifo = true,
    visibility = Seq(AddressSet(0x1000, 0xfff)))))))

  lazy val module = new LazyModuleImp(this) {
    // Creating Registers and mapping them to addresses
    val regs = RegInit(VecInit(Seq.fill(64)(0.U(64.W))))
    val tuples = regs.zipWithIndex.map { case (reg, i) =>
      (0x00 + (i * 8)) -> Seq(RegField(64,reg))
    }
    TLSlave.regmap(tuples :_*)
  }
}
```
Note that currently both `TLMaster` and `TLSlave` have to be defined, even if they are not used. This will be simplified later.

## Input Transaction Generation - Constrained Random
Verif also contains functionality to generate constrained random transactions. Below are two methods of doing so:

### Naive Random (Testing, will be deprecated soon)
The library contains a randomization function, `.rand()`, that automatically takes in a implicit `VerifRandomGenerator` (a custom random generator for Verif) and returns another instance of the Bundle type that it's called upon with randomized chisel fields. The function can also take a mapping of constraints for constrained random. The following is an example:

```scala
// Bundle definition for reference
case class InnerBundleNC[T <: Data](data: T, numb2: SInt = 0.S(8.W), numb3: UInt = 0.U(8.W)) extends Bundle
case class NestedBundleTxNC[T <: Data](data: T, inner1: InnerBundleNC[UInt], inner2: InnerBundleNC[UInt], numb1: UInt = 0.U) extends Bundle

class NoChiselRandomTest extends FlatSpec with Matchers {
  implicit val randGen: VerifRandomGenerator = new ScalaVerifRandomGenerator

  "Randomization Constraints" should "have no error" in {
    // Contraints in the form of a Map of fieldname (string) => list of boolean functions (constraints)
    var constraints: Map[String, ListBuffer[Data => Bool]] = Map[String, ListBuffer[Data => Bool]]()

    // Defining a transaction prototype, where randomized transactions will be created from
    var NTx_proto  = NestedBundleTxNC(255.U, InnerBundleNC(255.U, 100.S, 255.U), InnerBundleNC(255.U, 100.S, 255.U), 255.U)

    // Adding constraints to field named "data" (all UInts in this example)
    constraints += ("data" -> new ListBuffer[Data => Bool])
    constraints("data") += {v: Data => (v.litValue().toInt > 30).B}
    constraints("data") += {v: Data => (v.litValue().toInt < 100).B}

    // Adding another field "numb1"
    constraints += ("numb1" -> new ListBuffer[Data => Bool])
    constraints("numb1") += {v: Data => (v.litValue().toInt > 10 && v.litValue().toInt < 50).B}

    // Calling .rand() with constraints. The .printContents method just prints out the bundle contents
    NTx_proto.rand(constraints).printContents
    NTx_proto.rand(constraints).printContents
  }
}
```
For more example code using the `.rand()` method, please view `src/test/scala/NoChiselRandomTest.scala`.

---
Note that this method is called the "naive" random, as its approach to satisfying constraints is to keep generating random numbers until it finds one that is acceptable. While this is okay for basic sanity checks, it is by no means suitable for complex constrained randoms. The following section regarding SMT sampling will remedy this issue.

## Coverage
(TODO)
