package cosim

import chisel3._
import chisel3.util._
import java.net._
import java.io._
import scala.io._
import verif._

// A cosim driver server, recieves messages, translates them Bytes -> Message -> Bundle and pushes them into the driver
class CosimDriverServer[I, S, D](driver: AbstractDriver[I, S, D],
                              translator: (java.io.InputStream) => com.google.protobuf.Message,
                              bundleBuilder: (com.google.protobuf.Message) => D,
                              port: Int = 8008, timeout: Int = 15) extends Thread {
  val server = new ServerSocket(port)
  server.setSoTimeout(timeout*1000)
  override def run(): Unit = {
    try {
      while (true) {
        val s = server.accept()
        s.setSoTimeout(timeout*1000)
        val in = s.getInputStream()
        val out = new PrintStream(s.getOutputStream())

        val proto = translator(in)

        //TODO: should this part be forked to reduce server loop latency?
        // Store protos in queue and have forked operation that handles conversion and driving
        val bundle = bundleBuilder(proto)
        driver.push(driver.convertRawDataToStorage(bundle))

        // TODO: This sends properly, but can't be recieved by client without causing timeout
//        out.println(proto.toString())
//        out.flush()
        s.close()
      }
    } catch {
      case e: SocketException => println("Server: Socket Closed")
      case e: SocketTimeoutException => println("Server: Timed Out")
      case _: Throwable => println("Server: Unexpected Connection")
    }
  }

  def terminate: Unit = {
    server.close()
  }


}

object CosimClient {
  def sendString(str: String, port: Int = 8008): Unit = {
    val s = new Socket(InetAddress.getByName("localhost"), port)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

    out.println(str)
    out.flush()
    println("Received: " + in.next())

    s.close()
  }

  def sendProto(proto: com.google.protobuf.Message, port: Int = 8008, timeout: Int = 15): Unit = {
    try {
      val s = new Socket(InetAddress.getByName("localhost"), port)
      s.setSoTimeout(timeout * 1000)
      lazy val in = s.getInputStream()
      val out = s.getOutputStream()

      proto.writeTo(out)

      // TODO: This causes timeout...
//      val br = new BufferedReader(new InputStreamReader(in))
//      var read = br.readLine()
//      while (read != null) {
//        println(read)
//        read = br.readLine()
//      }

      s.close()
    } catch {
      case e: SocketTimeoutException => println("Client: Timed Out")
      case _: Throwable => println("Client: Unexpected Exception")
    }
  }
}