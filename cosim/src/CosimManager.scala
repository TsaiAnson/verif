package cosim

import chisel3._
import chiseltest._
import java.io.File
import java.nio.file.{Files, Paths}
import verif.{VerifCosimTestUtils, CosimTestDetails}


class CosimSimulator(simPath: String, simArgs: Seq[String], simTarget: String) extends Runnable {
  @volatile var exitCode = 0
  @volatile var stdOut = ""
  @volatile var stdErr = ""

  override def run: Unit = {
    val out = VerifCosimTestUtils.runCommand(simPath +: simArgs :+ simTarget)
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

class CosimManager(simPath: String, pipes: Seq[() => AbstractForkedCosimPipe], clock: Clock)(implicit cosimTestDetails: CosimTestDetails) {
  def run(simArgs: Seq[String], simTarget: String, correctnessCheck: Any => Boolean): Unit = {
    val path = s"${cosimTestDetails.testPath.get}/cosim_run_dir"

    /** IMPORTANT: Clean and re-create test directory **/
    Runtime.getRuntime().exec(s"rm -rf $path")
    Runtime.getRuntime().exec(s"mkdir $path")

    while (!Files.exists(Paths.get(path))) {
      Thread.sleep(500)
    }

    // Create and start sim thread (sim creates named pipes)
    val sim = new CosimSimulator(simPath, simArgs :+ s"--cosim-path=${path}", s"${cosimTestDetails.sbtRoot.get}/${simTarget}")
    val simThread = new Thread(sim)
    simThread.start

    // Create and start all pipe connectors
    pipes.map(pipe => pipe())

    // Wait for sim to terminate
    while (simThread.isAlive) {
      clock.step()
    }

    // Get simulation output
    val exitCode = sim.getExitCode
    val stdOut = sim.getStdOut
    val stdErr = sim.getStdErr

    println(exitCode)
    println(stdOut)
    println(stdErr)

    // Check correctness
    assert(correctnessCheck(exitCode))
  }
}
