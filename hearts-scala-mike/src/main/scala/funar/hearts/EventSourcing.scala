package funar.hearts

import cats.data._
import org.atnos.eff._
import org.atnos.eff.state._
import org.atnos.eff.syntax.all._
import org.atnos.eff.interpret._

sealed trait EventSourcing[Event, +A]

object EventSourcing {
  type _eventSourcing[Event, R] = EventSourcing[Event, *] |= R

  case class RecordEvent[Event](event: Event) extends EventSourcing[Event, Unit]
  case class CheckPointEvents[Event]() extends EventSourcing[Event, Seq[Event]]

  def recordEvent[Event, R: _eventSourcing[Event, *]](event: Event): Eff[R, Unit] =
    Eff.send[EventSourcing[Event, *], R, Unit](RecordEvent(event))
    
  def checkPointEvent[Event, R: _eventSourcing[Event, *]]: Eff[R, Seq[Event]] =
    Eff.send[EventSourcing[Event, *], R, Seq[Event]](CheckPointEvents())

  def runEventSourcingState[Event, R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[EventSourcing[Event, *], R, U],
              state: State[Seq[Event], *] |= U): Eff[U, A] = {
    translate(effects)(new Translate[EventSourcing[Event, *], U] {
      def apply[X](eventSourcing: EventSourcing[Event, X]): Eff[U, X] =
        eventSourcing match {
          case RecordEvent(event) =>
            for {
              _ <- modify[U, Seq[Event]] { (events: Seq[Event]) => events :+ event }
            } yield ()
          case CheckPointEvents() =>
            for {
              events <- get
              _ <- put[U, Seq[Event]](Seq.empty)
            } yield events
        }
    })
  }

  type IEventSourcing[A] = EventSourcing[Int, A]
  type SIState[A] = State[Seq[Int], A]
  type Stack = Fx.fx2[SIState, IEventSourcing]
  val e = checkPointEvent[Int, Stack]
  val f = runEventSourcingState(e)

}

object GameEventSourcing {

  type _gameEventSourcing[Effects] = Writer[Seq[GameEvent], *] |= Effects


  
}