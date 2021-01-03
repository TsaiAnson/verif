// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package verif

import maltese.smt
import org.scalatest.flatspec.AnyFlatSpec

class SMTSamplerTest extends AnyFlatSpec {
  behavior of "SMTSampler"

  it should "return a valid samples" in {
    val (a, b) = (smt.BVSymbol("a", 8), smt.BVSymbol("b", 8))
    val c0 = smt.BVComparison(smt.Compare.Greater, smt.BVLiteral(3, 8), a, signed = false)
    val c1 = smt.BVComparison(smt.Compare.Greater, b, smt.BVLiteral(3, 8), signed = false)

    val opt = SMTSamplerOptions(alphaMin = 0.5, maxCombine = 5, maxSamples = 40)
    val sampler = SMTSampler(List(a, b), List(c0, c1), opt, seed = 10).get
    val samples = sampler.run()
    assert(samples.size == opt.maxSamples)
    samples.map(_.toMap).foreach { sample =>
      //val aVal = sample("a")
      //val bVal = sample("b")
      //println(s"$aVal, $bVal")
      assert(sample.contains("a"))
      assert(sample("a") < 3)
      assert(sample.contains("b"))
      assert(sample("b") > 3)
    }
  }
}
