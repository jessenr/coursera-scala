package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case _ if c == r => 1
    case (c, r) => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def numberOfMatchedBraces(count: Int, charList: List[Char]): Int = (count, charList) match {
      case (_, Nil) => count
      case (0, ')' :: tail) => (-1)
      case (_, ')' :: tail) => numberOfMatchedBraces(count - 1, tail)
      case (_, '(' :: tail) => numberOfMatchedBraces(count + 1, tail)
      case (_, _) => numberOfMatchedBraces(count, charList.tail)
    }
    numberOfMatchedBraces(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def countChangeHelper(money:Int, sortedCoins:List[Int]): Int = {
      if(money < 0 || sortedCoins.isEmpty) {
    	  0
      } else if(money-sortedCoins.head == 0) {
    	  1
      } else {
    	  countChangeHelper(money - sortedCoins.head, sortedCoins) + countChangeHelper(money, sortedCoins.tail)
      }   
    }
    
    countChangeHelper(money, coins.sorted)
  }
}
