import org.junit.*
import org.junit.Assert.*

// Task 1 and 2 tests
class SequenceTest:
  import u03.Optionals.Optional.Just
  import u03.Optionals.Optional.Empty
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMapInTermsOfFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapFM(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapFM(l)(_ + ""))

  @Test def testFilterInTermsOfFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterFM(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterFM(l)(_ != 20))

  @Test def testMin() =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

// Task 3 Test
class Task3Test:
    import u03.Sequences.Sequence
    import u03.Sequences.Sequence.*
    import u02.Modules.Person.*
    import tasks.Task2.teacherCourses
    import u02.AlgebraicDataTypes.Person

    val teachers = Cons(Teacher("m", "math"), Cons(Teacher("e", "english"), Cons(Teacher("p", "programming"), Nil())))

    val students = Cons(Student("a", 2000), Cons(Student("b", 2001), Nil()))

    val expectedCourses = Cons("math", Cons("english", Cons("programming", Nil())))

    @Test def testTeacherCoursesExtraction() =
        assertEquals(expectedCourses, teacherCourses(teachers))

    @Test def testTeacherCoursesOnStudentsShouldYieldNil() =
        assertEquals(Nil(), teacherCourses(students))

    @Test def testTeacherCoursesOnTeachersAndStudentsShouldYieldTeachersCourses() =
        val concat = u03.Sequences.Sequence.concat(teachers, students)
        assertEquals(expectedCourses, teacherCourses(concat))

// Task 4 Test
class Task4Test:
    import u03.Sequences.Sequence.*
    import tasks.Task3.foldLeft
    
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))    

    @Test def testFoldLeftWithSum() =
        val expected = -16
        assertEquals(expected, foldLeft(lst)(0)(_ - _))

    @Test def testFoldLeftWithStringConcatenation() =
        val expected = "3715"
        assertEquals(expected, foldLeft(lst)("")(_ + _.toString()))

// Task 5 Test
class ExtensionSequenceTest:
  import u03.extensionmethods.Sequences.*
  import u03.extensionmethods.Sequences.Sequence.*
  import u03.extensionmethods.Optionals.Optional.None
  import u03.extensionmethods.Optionals.Optional.Just

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zip(l2))
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMapInTermsOfFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapFM(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapFM(l)(_ + ""))

  @Test def testFilterInTermsOfFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterFM(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterFM(l)(_ != 20))

  @Test def testMin() =
    assertEquals(Just(10), l.min())
    assertEquals(Just(1), Cons(1, Nil()).min())
    assertEquals(None(), Nil().min())

// Task 6/7/8 Tests
class StreamTest:
    import u03.Sequences.Sequence.Cons
    import u03.Sequences.Sequence.Nil
    import u03.Streams.Stream.*
    import u03.Streams.Stream
    
    @Test def testIterate(): Unit = 
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

    @Test def testMap(): Unit =
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
        assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

    @Test def testFilter(): Unit =
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
        assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

    @Test def takeWhile(): Unit = 
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

    @Test def testFill(): Unit =
        val str1 = Stream.toList(fill(3)("a"))
        assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), str1)

    @Test def testEmptyFill(): Unit =
        val empty = Stream.toList(fill(0)("a"))
        assertEquals(Nil(), empty)

    @Test def testPellNumbers(): Unit =
        val pellNumbers: Stream[Int] = pell()
        val firstFivePellNumbers = toList(Stream.take(pellNumbers)(5))
        val expectedNumbers = Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil())))))
        assertEquals(expectedNumbers, firstFivePellNumbers)