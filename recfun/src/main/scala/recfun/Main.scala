package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {

      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {

    if ((c == 0) || (c >= r)) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
    * Exercise 2
    * Recursively checks that a string contains a matching amount of opening
    * and closing parentheses by calling `balanced()` on the string without
    * the first element.
    *
    * Expectancy of parentheses in the string is kept in a kind of balance
    * indicator `open` - positives indicate amount of needed ')' and negatives
    * the amount of needed '('. Initial balance is `0`.
    *   
    * When recursion reaches the end of the string it checks if balance is 
    * OK (`open == 0`) 
    *
    * There is also a check (`open > 0`) to ensure that ')' was not encountered
    * before there was a '(' it could close.
    *
    * @param chars the string to be checked
    * @return true if string is balanced, false otherwise
    */
  def balance(chars: List[Char]): Boolean = {
    
    def balanced(chars: List[Char], open: Int): Boolean = {

      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail, open+1)
      else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)
    }

    balanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =

    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
