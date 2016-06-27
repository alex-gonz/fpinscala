package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = rng => map(int)(i => (i % (n + 1L)).toInt)(rng)

  @annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, rng2) if i > Int.MinValue => (i.abs, rng2)
    case (_, rng2) => nonNegativeInt(rng2)
  }

  def double(rng: RNG): (Double, RNG) = rng.nextInt match {
    case (i, rng2) => (i.toDouble / (Int.MaxValue + 1L), rng2)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) = map(int)(_.toDouble / (Int.MaxValue + 1L))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d,i),rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d,d2,d3),rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft[(List[Int], RNG)]((Nil, rng)) {
      case ((list, currentRng), _) =>
        val (i, newRng) = currentRng.nextInt
        (i :: list, newRng)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft[Rand[List[A]]](unit(Nil)) {
      case (randList, f) =>
        rng =>
          val (a, rng2) = f(rng)
          val (list, rng3) = randList(rng2)
          (a :: list, rng3)
    }
  }

  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, rng2) = f(rng)
      g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap[Int, Int](nonNegativeInt) { i =>
      rng =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
    }


  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      flatMap[A, C](ra){ a => rng =>
        val (b, rng2) = rb(rng)
        (f(a,b), rng2)
      }(rng)
  }
}

case class State[S,+A](run: S => (A, S)) {

  def unit[B](a: B): State[S, B] = State(s => (a, s))

  def map[B](f: A => B): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      (f(a), s2)
  }

  def mapWithFlatMap[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State {
    s =>
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a,b), s3)
  }

  def map2WithFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap { a =>
    State { s =>
      val (b, s2) = sb.run(s)
      (f(a,b), s2)
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      f(a).run(s2)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def output: (Int, Int) = (coins, candies)
}

object State {
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight[State[S, List[A]]](State(s => (Nil, s))) {
    case (f, b) => State { s =>
      val (a, s2) = f.run(s)
      val (as, s3) = b.run(s2)
      (a +: as, s3)
    }
  }
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State[Machine, (Int, Int)] { s =>
    val states = for {
      input <- inputs
    } yield doInput(input)

    val (_, machine) = sequence(states).run(s)
    (machine.output, machine)
  }

  def doInput(i: Input): State[Machine, (Int, Int)] = State { s =>
    val newS = i match {
      case Coin if s.candies > 0 => s.copy(locked = false, coins = s.coins + 1)
      case Turn if !s.locked && s.candies > 0 => s.copy(candies = s.candies - 1, locked = true)
      case _ => s
    }
    (newS.output, newS)
  }
}

object StateTests extends App {
  println(RNG.ints(5)(new RNG.Simple(0)))
  println(RNG.intsWithSequence(5)(new RNG.Simple(0)))
  println(State.simulateMachine(List(Turn, Coin,Turn, Coin,Turn, Coin)).run(Machine(locked = true, candies = 5, coins = 10)))
}