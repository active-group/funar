package funar.hearts

import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

case class Player(id: String, name: String) {
  override def equals(other: Any): Boolean =
    other match {
      case other: Player => this.id == other.id
      case _ => false
    }

  override def hashCode: Int = this.id.hashCode
}
