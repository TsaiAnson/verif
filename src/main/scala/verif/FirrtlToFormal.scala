// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package verif

import maltese.smt
import firrtl.annotations.DeletedAnnotation
import firrtl.backends.experimental.smt.ExpressionConverter
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.{AnnotationSeq, ir}
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage, OutputFileAnnotation, RunFirrtlTransformAnnotation}
import firrtl.transforms.NoCircuitDedupAnnotation
import firrtl.util.BackendCompilationUtilities
import logger.{LogLevel, LogLevelAnnotation}
import maltese.mc.TransitionSystem

object FirrtlToFormal  {
  def apply(c: ir.Circuit, annos: AnnotationSeq, ll: LogLevel.Value = LogLevel.Error): (TransitionSystem, AnnotationSeq) = {
    // TODO: ensure that firrtl.transforms.formal.AssertSubmoduleAssumptions is not run!

    val testDir = BackendCompilationUtilities.createTestDirectory(c.main + "_to_btor2")
    val combinedAnnos = Seq(
      LogLevelAnnotation(ll),
      FirrtlCircuitAnnotation(c),
      TargetDirAnnotation(testDir.getAbsolutePath),
      NoCircuitDedupAnnotation, // since we flatten everything anyways, there is no need to dedup.
    ) ++ annos
    val res = (new FirrtlStage).execute(Array("-E", "experimental-btor2"), combinedAnnos)
    val name = res.collectFirst { case OutputFileAnnotation(file) => file }
    assert(name.isDefined)

    val resAnnos = res.filterNot(_.isInstanceOf[DeletedAnnotation])

    // TODO: prevent btor file from being generated
    val btorFile = testDir.getAbsolutePath + s"/${name.get}.btor2"

    val sys = ExpressionConverter.toMaltese(resAnnos).getOrElse(throw new RuntimeException("Failed to find transition system annotation!"))
    (sys, resAnnos)
  }
}
