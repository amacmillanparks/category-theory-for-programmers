def optionToEither[A](option: Option[A]): Either[Unit, A] = option.fold[Either[Unit, A]](Left(()))(a => Right(a))

def eitherToOption[A](either: Either[Unit, A]): Option[A] = either.fold[Option[A]](_ => None, a => Some(a))

trait Shape {
  def area: Double 
  def circ: Double
}

class Circle(radius: Double) extends Shape {
  override def area: Double = Math.PI * radius * radius
  override def circ: Double = 2.0d * Math.PI * radius
}

class Rect(length: Double, width: Double) extends Shape {
  override def area: Double = length * width
  override def circ: Double = 2.0d * (length + width)
}

class Square(length: Double) extends Shape {
  override def area: Double = length * length
  override def circ: Double = 4.0d * length
}

def aPlusAToTwoTimesA[A](e: Either[A, A]): (Boolean, A) = e.fold(la => (false, la), ra => (true, ra))

def twoTimesAtoAPlusA[A](product: (Boolean, A)): Either[A, A] = Either.cond(product._1, product._2, product._2)
