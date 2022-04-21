sealed trait Maybe[+A] extends Product with Serializable {
    def id: Maybe[A] = this
}
final case object None extends Maybe[Nothing]
final case class Some[A](a: A) extends Maybe[A]

object Maybe {
    def compose[A, B, C](f: A => Maybe[B], g: B => Maybe[C]): A => Maybe[C] =
        a => f(a) match {
            case None => None
            case Some(b) => g(b)
            }

    def safeReciprocal(n: Double): Maybe[Double] = if (n == 0) None else Some(1 / n)

    def safeRoot(n: Double): Maybe[Double] = if (n >= 0) Some(Math.sqrt(n)) else None

    def safeRootReciprocal(n: Double): Maybe[Double] = safeReciprocal(n) match {
        case None => None
        case Some(a) => safeRoot(a)
    }
}
