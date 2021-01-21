package verif

import org.scalatest.flatspec.AnyFlatSpec

import designs._
import cosim._
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{RoCCCommand}
import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, PrintStream, BufferedReader, InputStreamReader}
import java.util.stream.Collectors
import org.scalatest.matchers._
import scala.sys.process._
import scala.reflect.io.File
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.tile.{RoCCCommand, OpcodeSet}
import gemmini._

import com.verif._

class CosimTestRoCCReader(pipe: String) extends Runnable {
  @volatile private var terminate = false

  @volatile var messages = Seq[String]()
  @volatile var lastMessage = ""
  @volatile var i = 0

  override def run: Unit = {
    val in = new FileInputStream(pipe)

    while(!terminate) {
      val message = com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(in)

      if (message != null) {
        messages = messages :+ message.toString()
      }
    }
  }

  def exit: Unit = {
    terminate = true
  }

  def getMessages: Seq[String] = {
    messages
  }
}
class CosimTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  it should "Named Pipe Test" in {
    test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val fifoName = "TestFIFO"
      val fifoPath = s"${File(".").toAbsolute}/test_run_dir/should_Named_Pipe_Test/"
      val fifo = s"$fifoPath$fifoName"

      val cmd = RoCCProtos.RoCCCommand.newBuilder()
                .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
                .setInst(
                  RoCCProtos.RoCCInstruction.newBuilder()
                    .setFunct(2))
                .build()

      val write = new Thread {
        override def run: Unit = {
          val out = new FileOutputStream(fifo)
          cmd.writeDelimitedTo(out)
        }
      }

      val read = new Thread {
        override def run: Unit = {
          val in = new FileInputStream(fifo)
          val message = com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(in)
          println(message)
        }
      }

      println("Creating FIFO")
      Runtime.getRuntime().exec(s"mkfifo ${fifo}");
      Thread.sleep(1000)

      println("Starting Reader and Writer")
      write.start()
      read.start()

      Thread.sleep(1000)

      println("Stopping Reader and Writer")
      write.stop()
      read.stop()

      println("Deleting FIFO")
      Runtime.getRuntime().exec(s"rm ${fifo}");

      assert(true)
    }
  }

  it should "Read from Spike" in {
    /**IMPORTANT**/
    val path = "/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir" //TODO: get this automatically
    Runtime.getRuntime().exec(s"rm -rf $path")
    Runtime.getRuntime().exec(s"mkdir $path")

    while (!java.nio.file.Files.exists(java.nio.file.Paths.get(path))) {
      println("Waiting for cosim_run_dir to exist")
      Thread.sleep(500)
    }
    /**END IMPORTANT**/

    val simPath = "/home/rlund/adept/chipyard/esp-tools-install/bin/spike"
    val simArgs = Seq("--extension=gemmini")
    val simTarget = "/home/rlund/adept/chipyard/generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/mvin_mvout-baremetal"

    val sim = new CosimSimulator(simPath, simArgs, simTarget)
    val simThread = new Thread(sim)

    println("Starting sim thread")
    simThread.start

    // Spin until all FIFOs are created
    while (new java.io.File(path).listFiles.size < 1) {
      println("Waiting for FIFOs to exist")
      Thread.sleep(500)
    }


    // Create a reader
    val reader = new CosimTestRoCCReader("/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir/RoCCCommandPipe")
    val readerThread = new Thread(reader)

    println("Starting reader thread")
    readerThread.start



    // Wait for sim to terminate
    simThread.join

    // Get simulation output
    val exitCode = sim.getExitCode
    val stdOut = sim.getStdOut
    val stdErr = sim.getStdErr

    println(s"Exit code: $exitCode")
    println(s"StdOut: $stdOut")
    println(s"StdErr: $stdErr")

    // Since traffic is one way, spin a long time to let reader try and read
    Thread.sleep(10*1000)
    // Terminate reader
    reader.exit

    println(s"Reader saw ${reader.getMessages.size} messages:")
    println(reader.getMessages)
  }

  it should "Run Mvin-Mvout" in {
    val dut = LazyModule(
      new VerifRoCCStandaloneWrapper(
        () => new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig.copy(use_dedicated_tl_port = true,
          meshRows = 4, meshColumns = 4, rob_entries = 4)),
        beatBytes = 16,
        addSinks = 1
      ))

    val simPath = "/home/rlund/adept/chipyard/esp-tools-install/bin/spike"
    val simArgs = Seq("--extension=gemmini")
    val simTarget = "/home/rlund/adept/chipyard/generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/mvin_mvout-baremetal"

    test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      // Drivers
      val commandDriver = new DecoupledDriver[RoCCCommand](c.clock, c.io.cmd)

      // Monitors
      val tlMonitor = new TLMonitor(c.clock, c.tlOut(0))
      //c.tlOut(0).a.ready.poke(true.B) broken!

      val commandPipe = new RoCCCommandCosimPipeDriver(
        "/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir/RoCCCommandPipe",
        c.clock, commandDriver)

      val fencePipe = new FencePipe("/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir/GemminiFenceReqPipe",
        "/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir/GemminiFenceRespPipe", c.clock, c.io)

      //val tlPipe = new TLPipe("/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir/TLAPipe", "/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir/TLDPipe", c.clock);

      //val ptwRespDriver = new ValidDriver[PTWResp](c.clock, c.io.ptw(0).resp)
      // TODO: tlClientDriver is broken
      //val tlDriver = new TLClientDriverBasic(c.clock, dut.module.tlOut)

      // Monitors
      // val ptwReqMonitor = new DecoupledMonitor[ValidIO[PTWReq]](c.clock, c.io.ptw(0).req)
      // val tlMonitor = new TLClientMonitorBasic(c.clock, c.tlOut(0))

      val runner = new CosimRunner(simPath, Seq(commandPipe, fencePipe)); //, tlPipe));

      runner.run(simArgs, simTarget, x => x == 0)

      c.clock.step(500)

      // Print has nothing since monitor doesn't poke ready
      //println(tlMonitor.getMonitoredTransactions(verifTLUtils.filterA).map(tx => verifTLUtils.TLTransactiontoTLBundles(tx)))

      assert(true)
    }
  }
}
