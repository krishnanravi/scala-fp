package recfun

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == c || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val len = chars.length

    @tailrec
    def balanceIter(i: Int, unblanced: Int): Boolean = {
      if (i > len - 1) unblanced >= 0
      else {
        val c = chars(i)
        val unbalanced_recomp = c match {
          case '(' => unblanced + 1
          case ')' => unblanced - 1
          case _ => unblanced
        }

        if (unbalanced_recomp < 0) false
        else balanceIter(i + 1, unbalanced_recomp)
      }
    }

    balanceIter(0, 0)
  }

  /**
   * Exercise 3
   */
//  def countChange(money: Int, coins: List[Int]): Int = {
//    if(money <= 0 || coins.isEmpty) return 0
//    val distinctChanges = new mutable.ListBuffer[String]()
//    def countChangeIter(money: Int, count: Int, change: List[Int]): Int = {
//      if(money == 0) {
//        val changeStr = change.sorted.mkString((","))
//        if(!distinctChanges.contains(changeStr)) {
//          distinctChanges.append(changeStr)
//          println(changeStr)
//          count + 1
//        } else count
//      }
//      else coins.filter(_ <= money).
//              foldLeft(count)((acc, currDen) => countChangeIter(money-currDen, acc, change ::: List(currDen)))
//      }
//    countChangeIter(money, 0, List())
//    distinctChanges.length
//  }
//}

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}
