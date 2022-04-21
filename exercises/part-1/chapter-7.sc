trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
}

implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](f: A => B)(fa: Option[A]): Option[B] = fa match {
        case None => None
        case Some(a) => Some(f(a))
    }
}

case class Reader[R, A](run: R => A)

type IntReader[A] = Reader[Int, A]

implicit val intReaderFunctor: Functor[IntReader] = new Functor[IntReader] {
    override def fmap[A, B](f: A => B)(fa: IntReader[A]): IntReader[B] = 
        Reader { r => f(fa.run(r)) }
}

implicit def readerFunctor[R]: Functor[({ type T[A] = Reader[R, A]})#T] =
    new Functor[({ type T[A] = Reader[R, A]})#T] {
        override def fmap[A, B](f: A => B)(fa: Reader[R,A]): Reader[R,B] =
            Reader { r => f(fa.run(r)) }
    }
