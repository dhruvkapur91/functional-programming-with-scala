import simulacrum._

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def as[A, B](fa: F[A], b: => B): F[B] = lift((_: A) => b)(fa)

  def void[A](fa: F[A]): F[Unit] = as(fa, Unit)

  def compose[G[_]](implicit G : Functor[G]) : Functor[Lambda[X => F[G[X]]]] = {
    val self = this;
    new Functor[Lambda[X => F[G[X]]]] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = {
        self.map(fa)(G.map(_)(f))
      }
    }
  }
}

object Functor {

  implicit val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctor = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](fa: X => A)(f: A => B): X => B = fa andThen f
  }

}

trait FunctorLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) = {
    F.map(fa)(x => x) == fa
  }

  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) = {
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
  }
}

object Main extends App {

  import Functor._

  private val listF: Functor[List] = implicitly[Functor[List]]
  private val optionF: Functor[Option] = implicitly[Functor[Option]]
  private val functionF: Functor[Function[Int, ?]] = implicitly[Functor[Int => ?]]

  //  private val value1 = listF.compose(functionF) -- This is not working... it compiled yesterday... not compiling today... not sure what changed...

  //  private val intToStrings: List[Function[Int, String]] = listF.compose(functionF)
  //    .map(List.empty[Int => String])(x => x.toUpperCase)

  functionF.map(x => List(x))(x => x.map(_.toString))

  private val intToStrings1: Function[Int, List[String]] = functionF.compose(listF)
    .map(x => List(x))(x => x.toString)

  val data = List(Some(List(1,2,3)))

  listF.compose(optionF).compose(listF)
    .map(data)(x => x.toString)

  private implicit val value = listF.compose(optionF)

  println {
    listF.compose(optionF).map(List(Some(1), None, Some(2)))(x => x * 2)
  }

  val intToInt = (x: Int) => x
  private val intToString = implicitly[Functor[Int => ?]].map(intToInt)(x => s"x is even : ${x % 2 == 0}")
}