package funar.hearts

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.effect.IO
import org.atnos.eff.addon.cats.effect._
import org.atnos.eff.addon.cats.effect.IOEffect._
import org.atnos.eff.syntax.addon.cats.effect._
import cats.Traverse
// makes ms.map(apply) work
import cats.implicits._
import cats.effect.Console.io._

sealed trait Teletype[+A]

object Teletype {
  def readLine[T, R : _io]: Eff[R, String] = 
    fromIO(readLn)
  def writeLine[T, R : _io](line: String): Eff[R, Unit] =
    fromIO(putStrLn(line))

  case object ReadTTY extends Teletype[String]
  case class WriteTTY(line: String) extends Teletype[Unit]
  
  type _teletype[R] = Teletype |= R

  def readTTY[T, R : _teletype]: Eff[R, String] =
    Eff.send[Teletype, R, String](ReadTTY)

  def writeTTY[T, R : _teletype](line: String): Eff[R, Unit] =
    Eff.send[Teletype, R, Unit](WriteTTY(line))

  def p[R: _teletype]: Eff[R, String] =
    for {
      _ <- writeTTY("First name:")
      _ <- writeTTY("Really")
      first <- readTTY
      _ <- writeTTY("Surname:")
      sur <- readTTY
      - <- writeTTY("Hello " + first + " " + sur)
    } yield (first + sur)

  def runTeletypeUnsafe[R, A](effects: Eff[R, A])(implicit m: Teletype <= R): Eff[m.Out, A] = {
    val sideEffect = new SideEffect[Teletype] {
      def apply[X](tty: Teletype[X]): X =
        tty match {
          case WriteTTY(line) =>
            println(line)
            ().asInstanceOf[X]
          case ReadTTY =>
            println("reading")
            (scala.io.StdIn.readLine()).asInstanceOf[X]
        }

      def applicative[X, Tr[_] : Traverse](ms: Tr[Teletype[X]]): Tr[X] =
        ms.map(apply)
    }
    interpretUnsafe(effects)(sideEffect)(m)
  }

  // R is U + Teletype - IO
  def runTeletype[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[Teletype, R, U],
              io: _io[U]): Eff[U, A] = {
            
    translate(effects)(new Translate[Teletype, U] {
      def apply[X](tty: Teletype[X]): Eff[U, X] =
        tty match {
          case ReadTTY => 
            for {
              line <- readLine
            } yield line
          case WriteTTY(line) =>
            for {
              _ <- writeLine(line)
            } yield ()
        }
    })
  }
  
  type Stack = Fx.fx2[IO, Teletype]

  def runP() = unsafeRunSync(runTeletype(p[Stack]))


}
