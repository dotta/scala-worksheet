package org.scalaide.worksheet.runtime

import scala.actors.{ Actor, DaemonActor }
import scala.collection.mutable
import scala.tools.eclipse.ScalaProject
import org.scalaide.worksheet.ScriptCompilationUnit
import scala.tools.eclipse.logging.HasLogger

object WorksheetsManager {
  lazy val Instance: Actor = {
    val proxy = new WorksheetsManager
    proxy.start()
    proxy
  }
}

private class WorksheetsManager private extends DaemonActor with HasLogger {
  import WorksheetRunner.RunEvaluation
  import ProgramExecutorService.StopRun

  private case class WorksheetState(scalaProject: ScalaProject, worksheetRunner: Actor)

  private var activeWorksheetRunner: WorksheetState = _

  override def act() = loop {
    react {
      case msg @ RunEvaluation(unit, _) =>
        obtainEvaluator(unit) forward msg

      case msg: StopRun =>
        forwardIfEvaluatorExists(msg)

      case any => exit("Unsupported message " + any)
    }
  }

  private def forwardIfEvaluatorExists(msg: StopRun): Unit = {
    if (activeWorksheetRunner == null) logger.info("No active worksheet runner. Ignoring " + msg)
    else {
      val scalaProject = msg.unit.scalaProject
      if (scalaProject != activeWorksheetRunner.scalaProject) logger.info("No running evaluation for worksheet `" + msg.unit.file.name + "`. Ignoring " + msg)
      else {
        assert(activeWorksheetRunner.scalaProject == scalaProject)
        activeWorksheetRunner.worksheetRunner forward msg
      }
    }
  }

  private def obtainEvaluator(unit: ScriptCompilationUnit): Actor = {
    val scalaProject = unit.scalaProject
    def createNewRunner(): Actor = {
       val evaluator = WorksheetRunner(scalaProject)
      WorksheetState(scalaProject, evaluator)
      evaluator
    }
    
    if (activeWorksheetRunner == null) createNewRunner()
    else if(activeWorksheetRunner.scalaProject == scalaProject) activeWorksheetRunner.worksheetRunner
    else {
      activeWorksheetRunner.worksheetRunner ! WorksheetRunner.Stop
      createNewRunner()
    }
  }

  override def toString: String = "WorksheetManager <actor>"
}