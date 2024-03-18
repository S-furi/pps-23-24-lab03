package tasks

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u02.Modules.Person
import u02.Modules.Person.*

object Task2:
    def teacherCourses(l: Sequence[Person]): Sequence[String] = flatMap(l)(x => x match
            case Teacher(name, course) => Cons(course, Nil())
            case _ => Nil()
        )