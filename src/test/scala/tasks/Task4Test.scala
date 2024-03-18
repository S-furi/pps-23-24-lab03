package tasks

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import tasks.Task3.foldLeft

class Task4Test:
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))    

    @Test def testFoldLeftWithSum() =
        val expected = -16
        assertEquals(expected, foldLeft(lst)(0)(_ - _))

    @Test def testFoldLeftWithStringConcatenation() =
        val expected = "3715"
        assertEquals(expected, foldLeft(lst)("")(_ + _.toString()))