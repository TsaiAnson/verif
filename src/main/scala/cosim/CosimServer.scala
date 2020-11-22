package cosim

import chisel3._
import chisel3.util._
import java.net._
import java.io._
import scala.io._
import verif._

class CosimServer(port: Int = 8008) extends Thread {
  val server = new ServerSocket(port)
  override def run(): Unit = {
    try {
      while (true) {
        val s = server.accept()
        val in = new BufferedSource(s.getInputStream()).getLines()
        val out = new PrintStream(s.getOutputStream())

        out.println(in.next())
        out.flush()
        s.close()
      }
    } catch {
      case e: SocketException => println("Socket Closed")
      case _: Throwable => println("Unknown Exception Occured")
    }
  }

  def terminate(): Unit = {
    server.close()
  }


}

object CosimClient {
  def main(port: Int = 8008, str: String) : Unit = {
    val s = new Socket(InetAddress.getByName("localhost"), port)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

    out.println(str)
    out.flush()
    println("Received: " + in.next())

    s.close()
  }
}