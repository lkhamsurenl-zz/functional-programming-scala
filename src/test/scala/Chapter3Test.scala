/**
 * Created by luvsandondov on 8/12/15.
 */

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Chapter3Test extends AssertionsForJUnit {
  @Test def DropTest(): Unit = {
    val got = Drop.dropWhile1(List(1,2,3))(x => x == 1)
    assert(got == List(2,3), s"dropWhile(List(1,2,3), x => x == 1): ${got}; want: List(2,3)")
  }
  @Test def FoldTest() {
    // Check if the length is 3.
    assert(3 === Fold.length(List(1,2,3)))
    assert(List(1,2,3) === Fold.reverse(List(3,2,1)))
    // Merge lists should append list of lists:
    assert(Fold.mergeLists(List(List(1,2,3), List(4,5,6), List(7,8,9))) == (1 to 9).toList, Fold.mergeLists(List(List(1,2,3), List(4,5,6), List(7,8,9))))
  }
  @Test def MapTest() {
    assert((List(2,3,4)) === Map.myMap(List(1,2,3))(1 + _))
    assert(List(1,2,3,4,5) === Map.myFilter((1 to 10).toList)(_ < 6))
    // ensure flatMap works as intended:
    val got = Map.myFlatMap(List(1,2,3))((x: Int) => List(1,2,3).map(_ * x))
    assert(got == List(1,2,3,2,4,6,3,6,9))
  }
  // No empty tree
  @Test def TreeTest(): Unit = {
    val depth0Tree = Leaf(1)
    val depth1Tree = Branch(1, Leaf(2), Leaf(3))
    val depth2Tree = Branch(1, Leaf(2), Branch(3, Leaf(4), Leaf(5)))
    // size of tree
    assert(Tree.size(depth0Tree) === 1)
    assert(Tree.size(depth2Tree) === 5)
    // maximum element
    assert(Tree.maximum(depth0Tree) === 1)
    assert(Tree.maximum(depth2Tree) === 5)
    
    // depth of tree
    assert(Tree.depth(depth0Tree) === 0)
    assert(Tree.depth(depth1Tree)===1)
    assert(Tree.depth(depth2Tree) === 2)
    
    // map 
    val expectedTree = Branch(2, Leaf(3), Branch(4, Leaf(5), Leaf(6)))
    assert(Tree.map(depth2Tree, (v: Int) => v + 1) == expectedTree)
  }
}
