package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println(balance("())(".toList))
    println(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if(c==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def lefted_paren(count: Int, right_list : List[Char]): Int = {
      if (!right_list.isEmpty && count >= 0) {
//        println(count)
        if (right_list.head == '(') lefted_paren(count + 1, right_list.tail)
        else if (right_list.head == ')') lefted_paren(count - 1, right_list.tail)
        else lefted_paren(count, right_list.tail)
      }
      else count
    }
    lefted_paren(0,chars)==0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money==0) 1
    else if(!coins.isEmpty && money>0) countChange(money-coins.head, coins) + countChange(money,coins.tail)
    else 0
  }
}
