package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Leaf('a',2)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of leaf") {
    new TestTrees {
      assert(chars(t3) === List('a'))
    }
  }

  test("times of list") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
  }

  test("times") {
    new TestTrees {
      val pairs = times(List('a', 'b', 'c', 'a', 'a'))
      assert(pairs.size == 3)
      assert(pairs.exists(pair => pair._1 == 'a' && pair._2 == 3))
    }
  }

  test("times of nil") {
    new TestTrees {
      assert(times(Nil) === List())
    }
  }

  test("createCodeTree") {
    new TestTrees {
      val tt1 = createCodeTree("aabbb".toList);
      val tt2 = createCodeTree("aabbbdddd".toList)
      assert(t1 === tt1)
      assert(t2 === tt2)
    }
  }

  test("create code tree"){
    assert(createCodeTree("helloworld".toCharArray.toList) === Fork(Fork(Fork(Leaf('w',1),Leaf('r',1),List('w', 'r'),2),Leaf('o',2),List('w', 'r', 'o'),4),Fork(Fork(Leaf('d',1),Fork(Leaf('h',1),Leaf('e',1),List('h', 'e'),2),List('d', 'h', 'e'),3),Leaf('l',3),List('d', 'h', 'e', 'l'),6),List('w', 'r', 'o', 'd', 'h', 'e', 'l'),10))
  }

  test("test encode french") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("convert to CodeTable"){
    assert(convert(createCodeTree("helloworld".toCharArray.toList)) === List(('w',List(0, 0, 0)), ('r',List(0, 0, 1)), ('o',List(0, 1)), ('d',List(1, 0, 0)), ('h',List(1, 0, 1, 0)), ('e',List(1, 0, 1, 1)), ('l',List(1, 1))))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of singleton or nil") {
    new TestTrees {
      assert(combine(Nil) === Nil)
      assert(combine(List(Leaf('a',2))) === List(Leaf('a',2)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode") {
    val secret: List[Bit] = List(0, 0, 1, 1,0,1)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    assert(decode(t1, secret) === "aabbab".toList)
  }
  
  test("encode") {
    val secret: List[Char] = List('a', 'a', 'b', 'b', 'a', 'b')
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val a = List[Bit](0,0,1,1,0,1)
    assert(encode(t1)(secret) === a)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val table = List( ('a',List(0,0)), ('b',List(0,1)), ('c',List(1,0)), ('d',List(1,1)) )
    assert(codeBits(table)('c') === List(1,0))
  }

  test("convert") {
    val tree = Fork( Fork( Leaf('c',1), Leaf('b',2), List('c', 'b'),3), Leaf('a',3), List('c', 'b', 'a'), 6)
    val table = List( ('c',List(0,0)), ('b',List(0,1)), ('a',List(1)) )
    assert(convert(tree) === table)
  }

  test("quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("ultimate test"){
    val tree = createCodeTree("The quick brown fox jumps over the lazy dog".toCharArray.toList)
    println(quickEncode(tree)("is this right?".toCharArray.toList))
  }
}
