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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def parenthesis(open: Int, chars: List[Char]): Boolean = {

      def calcOpen(char: Char): Int = {
        if (char == '(' && open >= 0) open + 1
        else if (char == ')') open - 1
        else open
      }

      if (chars.size == 0) open == 0
      else parenthesis(calcOpen(chars.head), chars.tail)
    }

    parenthesis(0, chars)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(acc: Int, money:Int, coins: List[Int]) : Int = {
       if (money == 0) acc + 1
       else if (coins.size == 0 || money < 0) acc
       else acc + count(acc, money - coins.head, coins) + count(acc, money, coins.tail)
    }

    count(0, money, coins)

  }
}
