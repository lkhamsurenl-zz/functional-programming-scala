/**
 * Created by luvsandondov on 8/12/15.
 */

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Chapter3Test extends AssertionsForJUnit {
  @Test def FoldTest() {
    assert(List(1,2,3) === Fold.reverse(List(3,2,1)))
  }
  @Test def MapTest() {
    assert((List(2,3,4)) === Map.myMap(List(1,2,3))(1 + _))
    assert(List(1,2,3,4,5) === Map.myFilter((1 to 10).toList)(_ < 6))
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
