package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = sys.error("todo")

  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = sys.error("todo")

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(l, f(z, x))(f)
    }
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight[Int, List[Int]](l, Nil)((a, b) => Cons(a+1, b))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight[Double, List[String]](l, Nil)((a, b) => Cons(a.toString, b))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight[A, List[B]](l, Nil)((a, b) => Cons(f(a), b))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight[A, List[A]](l, Nil)((a, b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight[A, List[B]](l, Nil)((a, bs) => append(f(a), bs))
//    foldRight[A,List[B]](l, Nil)((a, bs) => foldRight[B,List[B]](f(a), bs)((b,bs) => Cons(b,bs)))
  }

  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap[A,A](l)(a => if (f(a)) List(a) else Nil)
  }

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Nil, Cons(x, xs)) => Cons(x, xs)
      case (Cons(x, xs), Nil) => Cons(x, xs)
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipAdd(xs, ys))
    }
  }

  def combine[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Nil, Cons(x, xs)) => Cons(x, xs)
      case (Cons(x, xs), Nil) => Cons(x, xs)
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), combine(xs, ys)(f))
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (Nil, _) => false
      case (Cons(_, xs), Cons(_, ys)) => firstAreEqual(l, sub) || hasSubsequence(xs, ys)
    }
  }

  def firstAreEqual[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) if x == y => firstAreEqual(xs, ys)
      case _ => false
    }
  }
}
