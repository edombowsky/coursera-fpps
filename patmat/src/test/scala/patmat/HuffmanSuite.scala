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
    val helloTree = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a leaf") {
    assert(weight(Leaf('a',3)) === 3)
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

  test("times of empty List") {
    new TestTrees {
      assert(times(List()) === List())
    }
  }

  test("times of [a,a,a,a,a]") {
    assert(times(List('a','a','a','a','a')) === List(('a',5)))
  }

  test("times of a longer word") {
    assert(times(string2Chars("some-longer-and-weird-word-wtf"))
      === List(('s',1),('o',3),('m',1),('e',3),('-',5),('l',1),('n',2),('g',1),('r',3),('a',1),('d',3),('w',3),('i',1),('t',1),('f',1)))
  }

  test("an empty list is NOT a singleton") {
    new TestTrees {
      assert(singleton(List()) === false)
    }
  }

  test("a list with more than one element is NOT a singleton"){
    assert(singleton(List(Leaf('a', 1), Leaf('b',2))) === false)
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
    }
  }

  test("not a singleton") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("combine until singleton of some leaf list") {
    new TestTrees {
      val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      val expectedLeafList = List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7))
      assert(until(singleton, combine)(leaflist) === expectedLeafList)
    }
  }

  test("until should reduce to a singleton") {
    val leaflist = List(Leaf('b', 2), Leaf('c', 3), Leaf('d', 4), Leaf('f', 6), Leaf('g', 7))
    assert(singleton(until(singleton, combine)(leaflist)))
  }

  test("createCodeTree from some chars") {
    val chars = string2Chars("ettxxxx")
    assert(createCodeTree(chars) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7))
  }

  test("create code tree") {
    assert(createCodeTree(List('a','b','a')) === Fork(Leaf('b',1), Leaf('a',2), List('b','a'),3))
  }

  test("test encode french") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("encode using french code") {
    assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
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
      assert(combine(List()) === List())
      assert(combine(List(Leaf('a',2))) === List(Leaf('a',2)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))

    val leaflist1 = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist1) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))

    val leaflist2 = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('z', 3), Leaf('x', 4))
    assert(combine(leaflist2) === List(Leaf('x',4),
        Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('z',3),List('e', 't', 'z'),6)))
  }

  test("decode secret") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
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

  test("creating a full huffman tree from a list of characters") {
    new TestTrees {
      assert(createCodeTree("hello".toList) === helloTree)
    }
  }

  test("decoding an 'l' from the 'hello' tree") {
    new TestTrees {
      assert(decode(helloTree, List(0)) === List('l'))
    }
  }

  test("decoding an 'o' from the 'hello' tree") {
    new TestTrees {
      assert(decode(helloTree, List(1,0)) === List('o'))
    }
  }

  test("decoding an 'e' from the 'hello' tree") {
    new TestTrees {
      assert(decode(helloTree, List(1,1,1)) === List('e'))
    }
  }

  test("decoding an 'h' from the 'hello' tree") {
    new TestTrees {
      assert(decode(helloTree, List(1,1,0)) === List('h'))
    }
  }

  test("decoding 'lo' from the 'hello' tree") {
    new TestTrees {
      assert(decode(helloTree, List(0,1,0)) === List('l','o'))
    }
  }

  test("decoding 'hello' from the 'hello' tree") {
    new TestTrees {
      assert(decode(helloTree, List(1,1,0,1,1,1,0,0,1,0)) === List('h','e','l','l','o'))
    }
  }

  test("decoding the secret with the frenchcode tree"){
    assert(decode(Huffman.frenchCode, Huffman.secret) === "huffmanestcool".toList)
  }

  test("encoding 'l' from the 'hello' tree") {
    new TestTrees {
      assert(encode(helloTree)(List('l')) === List(0))
    }
  }

  test("encoding 'o' from the 'hello' tree") {
    new TestTrees {
      assert(encode(helloTree)(List('o')) === List(1,0))
    }
  }

  test("encoding 'e' from the 'hello' tree") {
    new TestTrees {
      assert(encode(helloTree)(List('e')) === List(1,1,1))
    }
  }

  test("encoding 'h' from the 'hello' tree") {
    new TestTrees {
      assert(encode(helloTree)(List('h')) === List(1,1,0))
    }
  }

  test("encoding 'hello' from the 'hello' tree") {
    new TestTrees {
      assert(encode(helloTree)(List('h','e','l','l','o')) === List(1,1,0,1,1,1,0,0,1,0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }

 test("merging two code tables") {
    val one: CodeTable = List(('a', List(1,1,1)))
    val two: CodeTable = List(('e', List(1,1,0)))
    val merged: CodeTable = List(('a', List(1,1,1)), ('e', List(1,1,0)))
    assert(mergeCodeTables(one, two) === merged)
  }

  test("merging two code tables where one is empty") {
    val one: CodeTable = List()
    val two: CodeTable = List(('e', List(1,1,0)))
    assert(mergeCodeTables(one, two) === two)
  }

  test("merging two code tables both are the same table") {
    val one: CodeTable = List(('e', List(1)))
    val two: CodeTable = List(('e', List(0)))
    assert(mergeCodeTables(one, two) === List(('e', List(1)), ('e', List(0)))) // List(('e', List(1,0))))
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

  test("converting a codetree to a code table") {
    new TestTrees {
      val codetable: CodeTable = List(('l', List(0)), ('o', List(1, 0)), ('h', List(1, 1, 0)), ('e', List(1, 1, 1)))
      assert(convert(helloTree) === codetable)
    }
  }

  test("codeBits & convert") {
    new TestTrees {
      assert(codeBits(convert(t1))('a') === List[Bit](0))
      assert(codeBits(convert(t1))('b') === List[Bit](1))
      assert(codeBits(convert(t2))('a') === List[Bit](0, 0))
      assert(codeBits(convert(t2))('b') === List[Bit](0, 1))
      assert(codeBits(convert(t2))('d') === List[Bit](1))
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, quickEncode(t2)("badabad".toList)) === "badabad".toList)
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }

  test("quick encoding 'hello'") {
    new TestTrees {
      assert(quickEncode(helloTree)(List('h','e','l','l','o')) === List(1,1,0,1,1,1,0,0,1,0))
    }
  }

 def visualizeTree(tree: CodeTree) = {
    def visualizeTree0(tree: CodeTree, totalRows: Int, totalCols: Int): Unit = {
      try {
        def constructTree(tree: CodeTree, row: Int, col: Int, result: Array[Array[String]]): Array[Array[String]] = {
          def generateString(tree: CodeTree): String = tree match {
            case Fork(left, right, chars, weight) => "(" + chars.mkString + ":" + weight + ")"
            case Leaf(char, weight) => "[" + char + ":" + weight + "]"
          }
          def addIfFork(tree: CodeTree, row: Int, col: Int, result: Array[Array[String]]): Array[Array[String]] = tree match {
            case Fork(left, right, chars, weight) =>
              result(row + 1)(col) = "*" //limiter under fork (will be replaced)
              //decrement or increment columns based on number of chars in fork
              constructTree(left, row + 3, col - (chars.length - 1), result)
              constructTree(right, row + 3, col + (chars.length - 1), result)
            case _ => result //do nothing
          }
          if (row > 0) {
            result(row - 2)(col) = "#" //limiter two characters above leaf (will be replaced)
            result(row - 1)(col) = "|" //connector
          }
          result(row)(col) = generateString(tree) //display leaf or fork
          addIfFork(tree, row, col, result)
        }
        val initialArray = Array.fill(totalRows, totalCols) { "" }
        val visualizableTree = constructTree(tree, 0, totalRows / 2, initialArray)
        printTree(visualizableTree)
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
          visualizeTree0(tree, totalRows + 3, totalCols + 4) //tricrement rows, quadcrement cols and retry
      }
      def printTree(array: Array[Array[String]]): Unit = {
        def preserveWidth(field: String, colWidth: Int): String = {
          def preserveWidth(field: String, switchDirection: Boolean): String = {
            if (field.length > colWidth) field
            else if (switchDirection) preserveWidth(field + " ", !switchDirection)
            else preserveWidth(" " + field, !switchDirection)
          }
          preserveWidth(field, false)
        }
        def collapseCols(array: Array[Array[String]]): Array[Array[String]] = {
          def determineColWidth(array: Array[Array[String]]): Array[Int] = {
            val resultArray = Array.fill(totalCols) { 0 } //init columns to width 0
            for (r <- array.indices) {
              for (c <- array(r).indices) {
                if (array(r)(c).length > resultArray(c)) resultArray(c) = array(r)(c).length //increase column width if needed
              }
            }
            resultArray
          }
          val colWidths = determineColWidth(array)
          for (r <- array.indices) {
            for (c <- array(r).indices) {
              array(r)(c) = preserveWidth(array(r)(c), colWidths(c))
            }
          }
          array
        }
        def replaceLimiters(raw: String): String = {
          def replaceLimiters0(input: String, pattern: String, toRight: Boolean): String = {
            val builder = new StringBuilder(input)
            (pattern.r findAllIn input).matchData foreach { m =>
              for (s <- m.subgroups) {
                builder.delete(0, builder.length()).append(m.before)
                if (toRight) builder.append(" " + ("_" * (s.length - 3)) + "/*")
                else builder.append(" \\" + ("_" * (s.length - 3)) + " ")
                builder.append(m.after)
              }
            }
            (pattern.r findAllIn builder.toString).matchData foreach { m =>
              if (m.groupCount > 0) {
                val newLine = replaceLimiters0(builder.toString, pattern, toRight)
                builder.delete(0, builder.length()).append(newLine)
              }
            }
            builder.toString
          }
          replaceLimiters0(replaceLimiters0(raw, """([#][ ]*[*])""", true), """([*][ ]*[#])""", false)
        }
        collapseCols(array).foreach { line =>
          val rawOutput = line.mkString.replaceAll("""(?)\s+$""", "") //removes trailing spaces
          val printMe = replaceLimiters(rawOutput)
          if (printMe.length > 0) println(printMe)
        }
      }
    }

    visualizeTree0(tree, 1, 1)
  }
}
