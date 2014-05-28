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
  def pascal(c: Int, r: Int): Int = {
    if ((r == 0) || (c == 0) || (c == r)) 1 else {
    	pascal(c, r-1) + pascal(c-1, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(open: Int, chars: List[Char]): Boolean = {
    	chars match {
    	  case Nil => open == 0
    	  case x::tail => {
    		 if (x == '(')
    		   isBalanced(open+1, tail)
    		 else if (x == ')') {
    		   if (open == 0) false else isBalanced(open-1, tail)
    		 }
    		 else
    		   isBalanced(open, tail)
    	  }
    	}
    }
    isBalanced(0, chars)	
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    coins match {
      case Nil => 0
      case x::tail => {
        if (x == money) 1 + countChange(money,tail)
        else if (x > money) countChange(money, tail)
        else countChange(money-x, coins) + countChange(money, tail)
      }
    }
  }
}
