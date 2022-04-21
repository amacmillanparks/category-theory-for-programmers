// Show the isomorphism between the standard definition of Maybe and this desugaring:
// type Maybe' a = Either (Const () a) (Identity a)
// Hint: Define two mappings between the two implementations.
// For additional credit, show that they are the inverse of each other using equational
// reasoning.

case object Const
case class Identity[A](a: A)

type MyOption[A] = Either[Const.type, Identity[A]]

def fromOption[A](o: Option[A]): MyOption[A] = {
  o match {
    case None => Left(Const)
    case Some(a) => Right(Identity(a))
  }
}

def toOption[A](myOption: MyOption[A]): Option[A] = myOption match {
  case Left(_) => None
  case Right(Identity(a)) => Some(a)
}

// Let’s try another data structure. I call it a PreList because it’s a precursor to a List.
// It replaces recursion with a type parameter b.
// data PreList a b = Nil | Cons a b
// You could recover our earlier definition of a List by recursively applying PreList to
// itself (we’ll see how it’s done when we talk about fixed points).
// Show that PreList is an instance of Bifunctor.

sealed trait PreList[A, B]
case class MyNil[A, B]() extends PreList[A, B]
case class Cons[A, B](h: A, t: B) extends PreList[A, B]

object PreList {
  def first[A, B, C](l: PreList[A, B])(f: A => C): PreList[C, B] = l match {
    case MyNil() => MyNil[C, B]()
    case Cons(a, b) => Cons(f(a), b)
  }

  def second[A, B, D](l: PreList[A, B])(g: B => D): PreList[A, D] = l match {
    case MyNil() => MyNil[A, D]()
    case Cons(a, b) => Cons(a, g(b))
  }

  def bimap[A, B, C, D](l: PreList[A, B])(f: A => C, g: B => D): PreList[C, D] =
    second(first(l)(f))(g)
}
