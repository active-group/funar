package funar.hearts

import cats._
import cats.implicits._
import cats.data._
import cats.effect.IO
import cats.effect.std.Random

object Shuffle {
  def distribute[A](n: Int, list: List[A]): Seq[List[A]] = {
    def extract[A](list: List[A]): List[A] =
      list match {
        case Nil => Nil
        case (x :: xs) => 
          x :: extract(xs.drop(n-1))
      }
    (0 until n).map { i =>
      extract(list.drop(i))
    }
  }
}