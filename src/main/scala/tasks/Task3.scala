package tasks

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

object Task3:
    def foldLeft[A, B](l: Sequence[A])(start: B)(accumulator: (B, A) => B): B = l match
        case Cons(head, tail) => {
            val res = accumulator(start, head)
            foldLeft(tail)(res)(accumulator)
        }
        case _ => start
