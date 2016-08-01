package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft[M[List[A]]](unit(Nil))((mList, ma) => flatMap(mList)(list => map(ma)(_ :: list)))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft[M[List[B]]](unit(Nil))((mList, a) => flatMap(mList)(list => map(f(a))(_ :: list)))
    //sequence(la.map(f))

  // Generates a list of the value wrapped by the given monad
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = map(ma)(a => List.fill(n)(a))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldLeft[M[List[A]]](unit(Nil))((mList, a) => flatMap(f(a))(bool => if (bool) map(mList)(a :: _) else mList))

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = (a: A) => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
    // really helped to write out types for this one, and then compose's types
    // so I knew what the types of the functions were
    val h: M[A] => M[B] = compose[M[A], A, B](identity, f)
    h(ma)
  }

  // Failed proof for compose associative law == flatmap associative law
  // flatMap(flatMap(M[x])(f), g) == compose(f, g)(x)

  // flatMap(flatMap(Some(1))(f), g) == compose(f, g)(1)
  // flatMap(f(1), g) == ((a: A) => flatMap(f(a))(g))(1)
  // flatMap(f(1), g) == flatMap(f(1))(g))

  // compose(compose(f, g), h) == compose(f, compose(g, h))
  // compose((a2) => flatMap(f(a2))(g), h) == compose(f, (b) => flatMap(g(b))(h))
  // (a) => flatMap(((a2) => flatMap(f(a2))(g))(a)(h)
  // assume x.flatMap(f) == flatMap(x)(f)
  // (a) => flatMap(((a2) => f(a2).flatMap(g))(a))(h) ... tried and failed...


  // 11.10 compose(f, unit) == f
  // (a: A) => flatMap(f(a))(unit) == f
  // (a: A) => f(a) == f

  // compose(unit, f) == f
  // (a: A) => flatMap(unit(a))(f) == f
  // (a: A) => f(a) == f

  // 11.11 (a: A) => flatMap(f(a))(unit) == f
  // if f(a) = List(1,2,3):
  //    (a: A) => flatMap(List(1,2,3))(unit) == a => List(1,2,3)
  //    (a: A) => List(1,2,3) == a => List(1,2,3)

  // (a: A) => flatMap(unit(a))(f) == f
  // if f(a) = List(1,2,3):
  //    (a: A) => flatMap(List(a))(f) == a => List(1,2,3)
  //    (a: A) => List(1,2,3) == a => List(1,2,3)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
  def _compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => join(map(f(a))(g))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.chooser(ma)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  // Skipped the chapter
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = ???

    override def unit[A](a: => A): P[A] = ???
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f

    override def unit[A](a: => A): List[A] = List(a)
  }

  // Can't be done since it takes in 2 different types
  // Could maybe fix by making output type not a part of state?

  // ReplicateM will run S once for each
  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f
  }



  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)

    override def unit[A](a: => A): Id[A] = Id(a)
  }

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

// guessing flatmap = run st to generate from f and then run again to generate last reader
// sequence read all with same reader
// join read twice
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = new Reader[R,A](_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      new Reader[R,B](r => f(st.run(r)).run(r))
  }
}

object MonadTests extends App {
  val writeLastState = new State[Int, Int](i => (i, i+1))
  val replicated = Monad.stateMonad.replicateM(5, writeLastState)
  println(s"State run 5 times is ${replicated.run(0)}")
  val map2ed = Monad.stateMonad.map2(writeLastState, writeLastState)(_ + _)
  println(s"Map2ed is ${map2ed.run(0)}")
  val sequenced = Monad.stateMonad.sequence(List.fill(5)(writeLastState))
  println(s"sequenced is ${sequenced.run(0)}")
}

