// Copyright (c) 2017, Sanjit A. Seshia, Rohit Sinha and Pramod Subramanyan.
// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// this file originated from the UCLID5 Verification and Synthesis Engine

package maltese.smt.solvers.uclid

import scala.collection.JavaConverters._

class InteractiveProcess(args: List[String], saveInput: Boolean=false) {
  // create the process.
  private val cmdLine = (args).asJava
  private val builder = new ProcessBuilder(cmdLine)
  builder.redirectErrorStream(true)
  private val process = builder.start()
  private val out = process.getInputStream
  private val in = process.getOutputStream

  // stores what we've written to the interactive process so far
  private var inputString = ""
  override def toString = inputString

  // Is this the best way of telling if a process is alive?
  def isAlive : Boolean = {
    process.isAlive
  }

  // Some helper functions.
  private def stringToBytes(str: String): Array[Byte] = str.map(_.toChar).toCharArray.map(_.toByte)
  private def bytesToString(bytes: Array[Byte]) = new String(bytes)


  // Write to the process's input stream.
  def writeInput(str: String): Unit = {
    in.write(stringToBytes(str))
    if (saveInput) inputString += str
  }
  // Close stdin, this may cause the process to exit.
  def finishInput(): Unit = {
    in.flush()
    inputString = ""
    in.close()
  }
  // Read from the process's output stream.
  def readOutput() : Option[String] = {
    inputString = ""
    in.flush()
    var done = false
    while (!done) {
      if (!isAlive) {
        done = true
      }
      val numAvail = out.available()
      if (numAvail == 0) {
        Thread.sleep(5)
      } else {
        val bytes = Array.ofDim[Byte](numAvail)
        val numRead = out.read(bytes, 0, numAvail)
        val string = bytesToString ({
          if (numRead == numAvail) {
            bytes
          } else {
            bytes.slice(0, numRead)
          }
        })

        return Some(string)
      }
    }
    None
  }
  // Kill the process.
  def kill(): Unit = process.destroy()
}
