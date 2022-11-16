package pc.examples

import pc.utils.Time
import java.util.Random
import pc.examples.StochasticChannel.*

@main def mainStochasticChannelSimulation =
  Time.timed(
    println(stocChannel.newSimulationTrace(IDLE, new Random)
                           .take(10)
                           .toList
                           .mkString("\n")))

@main def computationOnSimulation =
  import pc.modelling.CTMCSimulation.*
  val trace = stocChannel.newSimulationTrace(IDLE, new Random)
  println(trace.take(10).toList.mkString("\n"))
  println(s"Time to first FAIL on a trace: ${trace.timeToState(FAIL)}")
  val average = stocChannel.averageTimeToState(using 100, new Random)(IDLE, FAIL)
  println(s"Average time to first FAIL: ${average}")