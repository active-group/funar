package funar.json

import io.circe._
import cats._
import cats.implicits._
import cats.data._
import scala.annotation.tailrec


object Decode {
  import Json._

  sealed trait Error
  object Error {
    case class Field(name: String, inner: Error) extends Error
    case class Index(index: Int, inner: Error) extends Error
    case class OneOf(inners: Seq[Error]) extends Error
    case class Failure(message: String, value: Json) extends Error
  }
  import Error._

  type Decoder[A] = Json => Either[Error, A]

  def string: Decoder[String] = { json =>
    json.asString.map(Right(_)).getOrElse(Left(Failure("not a string", json)))
  }

  def int: Decoder[Int] = { json =>
    json.asNumber.flatMap(_.toInt).map(Right(_)).getOrElse(Left(Failure("not an int", json)))
  }

  def double: Decoder[Double] = { json =>
    json.asNumber.map(_.toDouble).map(Right(_)).getOrElse(Left(Failure("not a double", json)))
  }

  def boolean: Decoder[Boolean] = { json =>
    json.asBoolean.map(Right(_)).getOrElse(Left(Failure("not a boolean", json)))
  }

  def nulld[A](value: A): Decoder[A] = { json =>
    if (json.isNull)
      Right(value)
    else
      Left(Failure("not null", json))
  }

  def list[A](element: Decoder[A]): Decoder[List[A]] = { json =>
    json.asArray match {
      case Some(vs) =>
        vs.toList.traverse(element)
      case None => Left(Failure("not a json array", json))
    }
  }

  def index[A](i: Int, element: Decoder[A]): Decoder[A] = { json =>
    json.asArray match {
      case Some(vs) =>
        if (i < vs.length)
          element(vs(i)).swap.map(Index(i, _)).swap
        else
          Left(Failure("index out of bounds: " + i, json))
      case None =>
          Left(Failure("not an array (index " + i + ")", json))
    }
  }

  def oneOf[A](alternatives: Decoder[A]*): Decoder[A] = { Json =>
    val results = alternatives.toIterable.map(_(Json))
    results.collectFirst { case result@Right(value) => result } match {
      case Some(result) => result
      case None => Left(Error.OneOf(results.map(_.swap.getOrElse(???)).toSeq))
    }
  }

  def field[A](name: String, decoder: Decoder[A]): Decoder[A] = { json =>
    json.asObject match {
      case Some(obj) =>
        obj(name) match {
          case None => Left(Failure("field " + name + " not found", json))
          case Some(entry) => decoder(entry).swap.map(Field(name, _)).swap
        }
      case _ => Left(Failure("not an object", json))       
    }
  }
 
}