/**
 * Created by lkhamsurenl on 1/12/16.
 */
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Chapter5Test extends AssertionsForJUnit {

  @Test def HeadOptionTest(): Unit = {
    val streams = List[Stream[Int]](Empty, Stream.constant(1), Stream(1,2,3))
    val wants = List[Option[Int]](None, Some(1), Some(1))
    for {(s, want) <- streams.zip(wants)} {
      val got = s.headOption
      assert(got == want, s"${s}.headOption = ${got} = want: ${want}")
    }
  }

  @Test def existTest(): Unit = {
    val streams = List[Stream[Int]](Empty, Stream.constant(1), Stream(1,2,3))
    val wants = List[Boolean](false, true, true)
    for {(s, want) <- streams.zip(wants)} {
      val got = s.exists(_ == 1)
      assert(got == want, s"${s}.exist(1) = ${got} = want: ${want}")
    }
  }
}
