package cosim

import java.io._
import verif._

abstract class AbstractCosimPipeDriver[T <: com.google.protobuf.Message, I, S, D](pipe: String, driver: AbstractDriver[I, S, D],
                                                                                  messageReader: (InputStream) => T,
                                                                                  bundleConverter: (com.google.protobuf.Message) => D ) {

  val in = new FileInputStream(pipe)

  val thread = new Thread() {
    override def run: Unit = {
      while(true) {
        val message = messageReader(in)
        driver.push(driver.convertRawDataToStorage(bundleConverter(message)))
      }
    }
  }

  def start(): Unit = {
    thread.start()
  }

  def stop(): Unit = {
    thread.stop()
  }
}

abstract class AbstractCosimPipeMonitor[T <: com.google.protobuf.MessageLite](pipe: String) {

  val out = new FileOutputStream(pipe)
}