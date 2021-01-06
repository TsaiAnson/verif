package cosim

import java.io.File
import java.nio.file.{Files, Paths}
import verif.VerifCosimTestUtils


class CosimSimulator(simPath: String, simArgs: Seq[String], simTarget: String) extends Runnable {

  @volatile var exitCode = 0
  @volatile var stdOut = ""
  @volatile var stdErr = ""

  override def run: Unit = {
    val out = VerifCosimTestUtils.runCommand(Seq(simPath, simArgs.mkString(" "), simTarget))
    exitCode = out._1
    stdOut = out._2
    stdErr = out._3
  }

  def getExitCode: Int = {
    exitCode
  }

  def getStdOut: String = {
    stdOut
  }

  def getStdErr: String = {
    stdErr
  }
}

class CosimRunner(simPath: String, drivers: Seq[AbstractCosimPipe], monitors: Seq[AbstractCosimPipe]) {

  def run(simArgs: Seq[String], simTarget: String, correctnessCheck: Any => Boolean): Unit = {
    val path = "/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir" //TODO: get this automatically

    /** IMPORTANT: Clean and re-create test directory **/
    Runtime.getRuntime().exec(s"rm -rf $path")
    Runtime.getRuntime().exec(s"mkdir $path")

    // Wait for directory to exist
    while (!Files.exists(Paths.get(path))) {
      println("Waiting for cosim_run_dir to exist")
      Thread.sleep(500)
    }


    // Create and start sim thread (sim creates fifos)
    val sim = new CosimSimulator(simPath, simArgs, simTarget)
    val simThread = new Thread(sim)

    simThread.start

    // Spin until all FIFOs are created
    while (new File(path).listFiles.size < (drivers ++ monitors).size) {
      println("Waiting for all FIFOs to exist")
      Thread.sleep(500)
    }


    // Create and start all driver / monitor runnables
    val threads = (drivers ++ monitors).map(cosimPipe => new Thread(cosimPipe))
    threads.foreach(thread => thread.start)

    // Wait for sim to terminate
    simThread.join

    // Get simulation output
    val exitCode = sim.getExitCode
    val stdOut = sim.getStdOut
    val stdErr = sim.getStdErr

    /** TEMPORARY **/
    // Spin to allow driver to recieve and push
    Thread.sleep(10*1000)
    /** END TEMPORARY **/

    // Terminate driver and monitor runnables
    (drivers ++ monitors).foreach(cosimPipe => cosimPipe.exit)

    // Check correctness
    correctnessCheck(exitCode)

    println(exitCode)
    println(stdOut)
    println(stdErr)
  }
}