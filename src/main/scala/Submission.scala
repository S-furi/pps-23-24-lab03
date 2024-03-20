
object Task1Sub:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*
    import u03.Optionals.Optional
    import u03.Optionals.Optional.*

    def zip[A, B](l1: Sequence[A], l2: Sequence[B]): Sequence[(A, B)] = (l1, l2) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
      case _ => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, l2))
      case (Nil(), Cons(h, t)) => Cons(h, t)
      case _ => Nil()

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()
    
    def mapFM[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(x => Cons(mapper(x), Nil()))
    
    def filterFM[A](l: Sequence[A])(predicate: A => Boolean): Sequence[A] = flatMap(l)(x => x match
      case x if predicate(x) => Cons(x, Nil())
      case _ => Nil()
    )
    
    // Task 2
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) => {
        min(t) match
            case Just(a) if h > a => min(t)
            case _ => Optional.Just(h)
      }
      case _ => Optional.Empty()

object Task3Sub:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*
    import u02.Modules.Person
    import u02.Modules.Person.*

    def teacherCourses(l: Sequence[Person]): Sequence[String] = flatMap(l)(_ match
            case Teacher(_, course) => Cons(course, Nil())
            case _ => Nil()
        )

object Task4Sub:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*

    def foldLeft[A, B](l: Sequence[A])(start: B)(acc: (B, A) => B): B = l match
        case Cons(head, tail) => {
            val curr = acc(start, head)
            foldLeft(tail)(curr)(acc)
        }
        case _ => start

object Task5Sub:
    import u03.extensionmethods.Optionals.Optional
    import u03.extensionmethods.Optionals.Optional.*
    object Sequences:

      enum Sequence[E]:
        case Cons(head: E, tail: Sequence[E])
        case Nil()

      object Sequence:

        extension (l: Sequence[Int])
          def sum: Int = l match
            case Cons(h, t) => h + t.sum
            case _          => 0

          def min(): Optional[Int] = l match
            case Cons(h, t) => {
                t.min() match
                    case Just(a) if h > a => t.min()
                    case _ => Optional.Just(h)
            }
            case _ => Optional.None()

        extension [A](l: Sequence[A])

          def map[B](mapper: A => B): Sequence[B] = l match
            case Cons(h, t) => Cons(mapper(h), t.map(mapper))
            case Nil()      => Nil()

          def filter(pred: A => Boolean): Sequence[A] = l match
            case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
            case Cons(_, t)            => t.filter(pred)
            case Nil()                 => Nil()

          def zip[B](second: Sequence[B]): Sequence[(A, B)] = (l, second) match
            case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
            case _ => Nil()

          def take(n: Int): Sequence[A] = l match
            case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
            case _ => Nil()

          def concat(l2: Sequence[A]): Sequence[A] = (l, l2) match
            case (Cons(h1, t1), _) => Cons(h1, t1.concat(l2))
            case (Nil(), Cons(h, t)) => Cons(h, t)
            case _ => Nil()

          def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
            case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
            case _ => Nil()

          def mapFM[B](mapper: A => B): Sequence[B] = l.flatMap(x => Cons(mapper(x), Nil()))

          def filterFM(predicate: A => Boolean): Sequence[A] = l.flatMap(x => x match
            case x if predicate(x) => Cons(x, Nil())
            case _ => Nil()
          )

        def of[A](n: Int, a: A): Sequence[A] =
          if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))

object Task678Sub:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*

    enum Stream[A]:
        private case Empty()
        private case Cons(head: () => A, tail: () => Stream[A])

    object Stream:

        def empty[A](): Stream[A] = Empty()

        def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
            lazy val head = hd
            lazy val tail = tl
            Cons(() => head, () => tail)

        def toList[A](stream: Stream[A]): Sequence[A] = stream match
            case Cons(h, t) => Sequence.Cons(h(), toList(t()))
            case _ => Sequence.Nil()

        def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
            case Cons(head, tail) => cons(f(head()), map(tail())(f))
            case _ => Empty()

        def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
            case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
            case Cons(head, tail) => filter(tail())(pred)
            case _ => Empty()

        def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
            case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
            case _ => Empty()

        def iterate[A](init: => A)(next: A => A): Stream[A] =
            cons(init, iterate(next(init))(next))

        // Task 6
        def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
            case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
            case _ => Empty()

        // Task 7
        def fill[A](n: Int)(k: A): Stream[A] = n match
            case n if n > 0 => cons(k, fill(n - 1)(k))
            case _ => Empty()
        
        // Task 8
        def pell(): Stream[Int] = Stream.map(iterate(0)(_ + 1))(pell_number)

        private def pell_number(n: Int): Int = n match
            case n if n == 0 => 0
            case n if n == 1 => 1
            case _ => 2 * pell_number(n - 1) + pell_number(n - 2)

    end Stream