import simulacrum._

trait Functor[F[_]] {
  def map[A,B](fa : F[A])(f : A => B) : F[B]
}

trait FunctorLaws {
  def identity[F[_],A](fa : F[A])(implicit F : Functor[F]) = {
    F.map(fa)(x => x) == fa
  }
}

object Main extends App {
  println("Everything compiles")
}