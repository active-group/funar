// Typklassen (Cats)

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

// x combine (y combine z) = (x combine y) combine z

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

/*
x combine empty = x
empty combine x = x
*/

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

/*
fa.fmap(identity) = identity
fa.fmap(g.andThen(h)) = fa.fmap(g).fmap(h)
*/

trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
}

/*
pure(identity).ap(´v) = v
pure(f).ap(pure(x)) = pure(f(x))
u.ap(pure(y)) = pure({f => f(y)}).ap(u)
pure({f => g => g.andThen(f)}).ap(u).ap(v).ap(w) = u.ap(v.ap(w))
*/

trait Monad[M[_]] extends Applicative[F[_]] {
  def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
}
/*
pure(a).flatMap(k)  =  k(a)
m.flatMap(pure)     =  m
m.flatMap { x => k(x).flatMap(h) } =  m.flatMap(k).flatMap(h)
*/
