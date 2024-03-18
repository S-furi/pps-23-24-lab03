package tasks

import org.junit.*
import org.junit.Assert.*

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u02.Modules.Person.*
import tasks.Task2.teacherCourses
import u02.AlgebraicDataTypes.Person


class Task3Test {
    val teachers = Cons(
        Teacher("m", "math"),
        Cons(
            Teacher("e", "english"),
            Cons(
                Teacher("p", "programming"),
                Nil()
            )
        )
    )

    val students = Cons(
        Student("a", 2000),
        Cons(
            Student("b", 2001),
            Nil()
        )
    )

    val expectedCourses = Cons("math", Cons("english", Cons("programming", Nil())))

    @Test def testTeacherCoursesExtraction() =
        assertEquals(expectedCourses, teacherCourses(teachers))

    @Test def testTeacherCoursesOnStudentsShouldYieldNil() =
        assertEquals(Nil(), teacherCourses(students))

    @Test def testTeacherCoursesOnTeachersAndStudentsShouldYieldTeachersCourses() =
        val concat = u03.Sequences.Sequence.concat(teachers, students)
        assertEquals(expectedCourses, teacherCourses(concat))
}