package fpinscala.laziness

import fpinscala.laziness.Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListWithFold: List[A] = foldRight[List[A]](Nil)((h,list) => h :: list)

  def take(n: Int): Stream[A] = n match {
    case i if i > 0 => this match {
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case _ => throw new IllegalArgumentException("Tried to take more elements than exist in a stream")
    }
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = n match {
    case i if i > 0 => this match {
      case Cons(h, t) => t().drop(n - 1)
      case _ => throw new IllegalArgumentException("Tried to drop more elements than exist in stream")
    }
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h, b) => b && p(h))

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((h, ret) => Cons(() => f(h), () => ret))

  def filter(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((h, ret) => if (p(h)) Cons(() => h, () => ret) else ret)

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight[Stream[B]](s)((h, ret) => Cons(() => h, () => ret))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((h, ret) => f(h).foldRight(ret)((h1, ret1) => Cons(() => h1, () => ret1)))

  def flatMap2[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((h, ret) => f(h).append(ret))

  def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeWithUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
    case (Empty, i) if i > 0 => throw new IllegalArgumentException("Tried to take more elements than are in Stream")
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithUnfold[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)) {
    case (Cons(h, t), Cons(h1, t1)) => Some((h(), h1()), (t(), t1()))
    case _ => None
  }

  def zipAllWithUnfold[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s) {
    case (Cons(h, t), Cons(h1, t1)) => Some((Some(h()), Some(h1())), (t(), t1()))
    case (Cons(h, t), _) => Some((Some(h()), None), (t(), Empty))
    case (_, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = unfold[Stream[A], Option[Stream[A]]](Some(this)) {
    case None => None
    case Some(s @ Cons(h, t)) => Some(s, Some(t()))
    case Some(_) => Some(Empty, None)
  }

  // Map can be done with unfold, but foldRight can't since it needs to be able to combine into a non-stream
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f)) // not using intermediate results

  // Can do some with unfold but probably wouldn't be able to use the recursive result since unfold builds from start to finish, but we need results from finish to start
  // Eg. Stream(1,2,3) needs result of Stream(1,2) => 5 to add the 1, so we don't redo a all the work
  def scanRightBetter[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h,t) =>
      val subResult = t().scanRightBetter(z)(f) // Generate intermediate result in val so we can use it
      Cons(() => f(h(), subResult.headOption.getOrElse(z)), () => subResult)
    case _ => Cons(() => z, () => Empty)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))

  def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))

  def fibs: Stream[Int] = {
    def fibsHelper(i: Int, i1: Int): Stream[Int] = Cons(() => i, () => fibsHelper(i1, i + i1))

    fibsHelper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => Cons(() => h, () => unfold(t)(f))
    case _ => Empty
  }

  def fibsWithUnfold: Stream[Int] = unfold((0, 1)) {
    case (h, h1) => Some((h, (h1, h + h1)))
  }

  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  def constantWithUnfold[A](a: A): Stream[A] = unfold(())(_ => Some(a, ()))

  def onesWithUnfold: Stream[Int] = unfold(())(_ => Some(1, ()))

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = s.zipAllWithUnfold(s2).foldRight(true) {
    case ((_, None), b) => b
    case ((a, a2), b) => b && a == a2
  }
}

object Foo {
  def main(args: Array[String]): Unit = {
    val s = Stream.fibsWithUnfold.take(8)
    println(s.toList)
    println(Stream.fromWithUnfold(2).take(9).toList)

    println("Map tests:")
    println(Stream.fibs.map(_ * 2).take(5).toList)
    println(Stream.fibs.mapWithUnfold(_ * 2).take(5).toList)

    println("startsWith tests:")
    println("(1,2,3) startsWith (1,2): " + Stream.startsWith(Stream(1,2,3), Stream(1,2)))
    println("(1,1,3) startsWith (1,2): " + Stream.startsWith(Stream(1,1,2), Stream(1,2)))
    println("(1,2) startsWith (1,2,3): " + Stream.startsWith(Stream(1,2), Stream(1,2,3)))

    println("tails tests:")
    println("Stream(1,2,3).tails as a list: " + Stream(1,2,3).tails.toList.map(_.toList))

    println("scanRight tests:")
    println("Stream(1,2,3).scanRight(0)(_ + _).toList: " + Stream(1,2,3).scanRight(0)(_ + _).toList)
    println("Stream(1,2,3).scanRightBetter(0)(_ + _).toList: " + Stream(1,2,3).scanRightBetter(0)(_ + _).toList)
  }
}

trait BookStream[+A] {
  def uncons: Option[(A, BookStream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A] = uncons match {
    case Some((h, t)) => h :: t.toList
    case _ => Nil
  }

  def take(n: Int): BookStream[A] = n match {
    case i if i > 0 => uncons match {
      case Some((h, t)) => BookStream.cons(h, t.take(n - 1))
      case _ => throw new IllegalArgumentException(s"$n is larger than BookStream's length")
    }
    case _ => BookStream.empty
  }

  def takeWhile(p: A => Boolean): BookStream[A] = uncons match {
    case Some((h, t)) if p(h) => BookStream.cons(h, t.takeWhile(p))
    case _ => BookStream.empty
  }

  def forAll(p: A => Boolean): Boolean = uncons match {
    case Some((h, t)) => p(h) && t.forAll(p)
    case _ => true
  }
}

object BookStream {
  def empty[A]: BookStream[A] =
    new BookStream[A] { def uncons = None }
  def cons[A](hd: => A, tl: => BookStream[A]): BookStream[A] =
    new BookStream[A] {
      lazy val uncons = Some((hd, tl))
    }
  def apply[A](as: A*): BookStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}