import simulacrum._

trait Functor[F[_]] {
  def map[A,B](fa : F[A])(f : A => B) : F[B]
}

trait FunctorLaws {
  def identity[F[_],A](fa : F[A])(implicit F : Functor[F]) = {
    F.map(fa)(x => x) == fa
  }

  def composition[F[_],A,B,C](fa : F[A], f : A=>B, g : B=>C)(implicit  F : Functor[F]) = {
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
  }
}

object Main extends App {
  println("Everything compiles")
}