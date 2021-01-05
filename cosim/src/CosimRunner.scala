package cosim

import verif.VerifCosimTestUtils
import java.io.File

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

    // Create and start sim thread (sim creates fifos)
    val sim = new CosimSimulator(simPath, simArgs, simTarget)
    val simThread = new Thread(sim)

    simThread.run

    // Spin until all FIFOs are created
    val path = "/home/rlund/adept/chipyard/tools/verif/cosim/cosim_run_dir" //TODO: get this automatically
    println(new java.io.File(path).listFiles.size)
    while (new java.io.File(path).listFiles.size < (drivers ++ monitors).size) {}


    // Create and start all driver / monitor runnables
    val threads = (drivers ++ monitors).map(cosimPipe => new Thread(cosimPipe))
    threads.foreach(thread => thread.start)

    // Wait for sim to terminate
    simThread.join

    // Get simulation output
    val exitCode = sim.getExitCode
    val stdOut = sim.getStdOut
    val stdErr = sim.getStdErr

    // Terminate driver and monitor runnables
    (drivers ++ monitors).foreach(cosimPipe => cosimPipe.exit)

    // Check correctness
    correctnessCheck(exitCode)

  }
}