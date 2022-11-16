package pc.modelling

import java.util.Random
import pc.utils.Stochastics

object CTMCSimulation:

  case class Event[A](time: Double, state: A)
  type Trace[A] = LazyList[Event[A]]

  export CTMC.*

  extension [S](self: CTMC[S])
    def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
      LazyList.iterate(Event(0.0, s0)) { case Event(t, s) =>
        if self.transitions(s).isEmpty
        then
          Event(t, s)
        else
          val choices = self.transitions(s) map (t => (t.rate, t.state))
          val next = Stochastics.cumulative(choices.toList)
          val sumR = next.last._1
          val choice = Stochastics.draw(next)(using rnd)
          Event(t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
      }

    def averageTimeToState(using numberOfRuns: Int, rnd: Random)(initial:S, to: S): Double =
      val times = (1 to numberOfRuns)
        .map(_ => self.newSimulationTrace(initial, rnd))
        .map(_.timeToState(to)).filter(_.isDefined)
      times.map(_.get).sum / times.size

  extension [S](trace: Trace[S])
    /**
     * Get the time to reach the state the first time.
     * It consider the initial state as the start
     * @param state state to reach.
     */
    def timeToState(state: S, maxElements: Int = 200, maxTime: Double = 100): Option[Double] =
      trace.take(maxElements).takeWhile(_.time <= maxTime).filter(_.state == state) match
        case l if l.isEmpty || l.last.time >= maxTime => None
        case l => Some(l.head.time)


