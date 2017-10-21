package recfun

object Main {
  def main(args: Array[String]) {
    print(balance("())(".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r < 2 || c < 1 || c > r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countOpen(count: Int, chars: List[Char]): Int = {
        if (chars.isEmpty) return count
        val first = chars.head
        if (first == ')') {
          if (count < 1) {
            -1
          } else {
            countOpen(count - 1, chars.tail)
          }
        } else if (first == '(') {
          countOpen(count + 1, chars.tail)
        } else {
          countOpen(count, chars.tail)
        }
      }
      countOpen(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) return 1
      if (coins.isEmpty) return 0
      val coin = coins.head

      if (coin > money) {
        countChange(money, coins.tail)
      } else if(coin == money) {
        1 + countChange(money, coins.tail)
      } else {
        countChange(money - coin, coins) + countChange(money, coins.tail)
      }
    }
  }
