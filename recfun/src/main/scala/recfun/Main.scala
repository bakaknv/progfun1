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
    * n == r
    * k == c
    *
    * (n, k) = (n-1, k-1) + (n-1, k)
    *
    */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0 || c < 0)
      0
    else if (c < 1 || r < 1 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    *
    **/
  def balance(chars: List[Char]): Boolean = {
    def balance0(counterLeft: Int, counterRight: Int, chars: List[Char]): Int = {
      if (counterRight > counterLeft) 1
      else chars match {
        case c :: cs =>
          if (c == '(') balance0(counterLeft + 1, counterRight, cs)
          else if (c == ')') balance0(counterLeft, counterRight + 1, cs)
          else balance0(counterLeft, counterRight, cs)
        case _ => counterLeft - counterRight
      }
    }
    balance0(0, 0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange0(money: Int, reversedCoins: List[Int]): Int = {
      reversedCoins match {
        case largest :: tail =>
          if (money - largest == 0)
            1 + countChange(money, tail)
          else if (money - largest > 0)
            countChange(money - largest, reversedCoins) + countChange(money, tail)
          else
            countChange(money, tail)
        case _ => 0
      }
    }
    countChange0(money, coins.sorted.reverse)
  }
}
