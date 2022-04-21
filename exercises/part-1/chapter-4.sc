sealed trait Either[+A, +B] extends Product with Serializable
final case class Left[A](v: A) extends Either[A, Nothing]
final case class Right[B](v: B) extends Either[Nothing, B]

object Injections {
    def i(n: Int): Int = n
    def j(b: Boolean): Int = if (b) 0 else 1
}

object Morphisms {
    def m(e: Either[Int, Boolean]): Int = e match {
        case Left(a) => Injections.i(a)
        case Right(b) => Injections.j(b)
    }

    def m1(de: DoubleEither[Int, Boolean]): Either[Int, Boolean] = de match {
        case DoubleLeft(a, _) => Left(a)
        case DoubleRight(b, _) => Right(b)
    }

    def m2(de: DoubleEither[Int, Boolean]): Either[Int, Boolean] = de match {
        case DoubleLeft(_, aa) => Left(aa)
        case DoubleRight(_, bb) => Right(bb)
    }
}

sealed trait DoubleEither[+A, +B] extends Product with Serializable
final case class DoubleLeft[A](a: A, aa: A) extends DoubleEither[A, Nothing]
final case class DoubleRight[B](b: B, bb: B) extends DoubleEither[Nothing, B]
