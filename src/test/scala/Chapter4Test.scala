/**
 * Created by lkhamsurenl on 1/10/16.
 */
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Chapter4Test extends AssertionsForJUnit {
  @Test def TraverseTest(): Unit = {
    var lst: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    var got = Either1.traverse(lst)(v => v * v)
    assert(got == Right(List(1,4,9)), got)

    lst = List(Right(1), Left("2nd val error"), Left("3rd value error"))
    got = Either1.traverse(lst)(v => v * v)
    assert(got == Left("2nd val error"), got)
  }

  @Test def SequenceTest(): Unit = {
    var lst: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    var got = Either1.sequence(lst)
    assert(got == Right(List(1,2,3)), got)

    lst = List(Right(1), Left("2nd val error"), Left("3rd value error"))
    got = Either1.sequence(lst)
    assert(got == Left("2nd val error"), got)
  }
}
