package org.nlogo.extensions

import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

package example {
  class ManhattanExtension extends DefaultClassManager {
    def load(manager: PrimitiveManager) {
      manager.addPrimitive("distance", Distance)
      manager.addPrimitive("distancexy", DistanceXY)
      manager.addPrimitive("face", Face)
      manager.addPrimitive("facexy", FaceXY)
    }
  }
}

package object example {
  def getCoords(agent: Agent): (Double, Double) =
    agent match {
      case t: Turtle => (t.xcor, t.ycor)
      case t: Patch => (t.pxcor.toDouble, t.pycor.toDouble)
  }

  def faceDir(pair1: (Double, Double), pair2: (Double, Double)): Double = {
    val (x1, y1) = pair1
    val (x2, y2) = pair2
    (x1 - x2, y1 - y2) match {
      case (x, y) if (math.abs(x) > math.abs(y) && x1 > x2) => 270
      case (x, y) if (math.abs(x) > math.abs(y) && x2 > x1) => 90
      case (x, y) if (math.abs(y) > math.abs(x) && y1 > y2) => 180
      case (x, y) if (math.abs(y) > math.abs(x) && y2 > y1) => 0
      case (x, y) if (math.abs(y) == math.abs(x)) => if(x1 > x2) { 270 } else if(x2 > x1) { 90 } else { 0 }
    }
  }

  object Distance extends DefaultReporter {
    override def getSyntax =
    reporterSyntax(Array(AgentType), NumberType, agentClassString = "-TP-")
    override def report(args: Array[Argument], context: Context): AnyRef = {
      val (x1, y1) = getCoords(context.getAgent)
      val (x2, y2) = getCoords(args(0).getAgent)

      Double.box(math.abs(x1 - x2) + math.abs(y1 - y2));
    }
  }

  object DistanceXY extends DefaultReporter {
    override def getSyntax =
    reporterSyntax(Array(NumberType, NumberType), NumberType, agentClassString = "-TP-")
    override def report(args: Array[Argument], context: Context): AnyRef = {
      val (x1, y1) = getCoords(context.getAgent)
      val (x2, y2) = (args(0).getDoubleValue, args(1).getDoubleValue)

      Double.box(math.abs(x1 - x2) + math.abs(y1 - y2));
    }
  }

  object Face extends DefaultCommand {
    override def getSyntax =
    commandSyntax(Array(AgentType), agentClassString = "-T--")
    override def perform(args: Array[Argument], context: Context): Unit = {
      context.getAgent.asInstanceOf[Turtle].heading(faceDir(getCoords(context.getAgent), getCoords(args(0).getAgent)))
    }
  }

  object FaceXY extends DefaultCommand {
    override def getSyntax =
    commandSyntax(Array(NumberType, NumberType), agentClassString = "-T--")
    override def perform(args: Array[Argument], context: Context): Unit = {
      context.getAgent.asInstanceOf[Turtle].heading(faceDir(getCoords(context.getAgent), (args(0).getDoubleValue, args(1).getDoubleValue)))
    }
  }
}
