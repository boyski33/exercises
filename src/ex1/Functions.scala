//Боян Кушлев F75128

package ex1

import scala.annotation.tailrec

object Functions {

  def length(data: List[Int]): Int = {
    @tailrec
    def loop(count: Int, data: List[Int]): Int = {
      if (data.isEmpty) count
      else {
        loop(count+1, data.tail)
      }
    }
    loop(0, data)
  }

  def ifelse(cond: Boolean, onTrue: Int, onFalse: Int): Int = {
    if (cond) return onTrue
    onFalse
  }


  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(count: Int, chars: List[Char]): Boolean = {
      if (count < 0) return false
      if (chars.isEmpty){
        if (count == 0) true
        else false
      }
      else if (chars.head == '(') {
        loop(count + 1, chars.tail)
      }
      else if (chars.head == ')') {
        loop(count - 1, chars.tail)
      }
      else
        loop(count, chars.tail)
    }
    loop(0, chars)
  }

  def map(chars: List[Char], f: (Char) => Any): List[Any] = {
    @tailrec
    def loop(chars: List[Char], res: List[Any]): List[Any] = {
      if (chars.isEmpty) res
      else{
        loop(chars.tail, res :+ f(chars.head))
      }
    }
    loop(chars, List())
  }

  def toUpperCase(chars: List[Char]): List[Char] = {
    def upperCase(char: Char): Char = {
      (char-32).toChar
    }

    @tailrec
    def loop(chars: List[Char], res: List[Char]): List[Char] = {
      if(chars.isEmpty) res
      else
        loop(chars.tail, res :+ upperCase(chars.head))
    }
    loop(chars, List())
  }

  @tailrec
  def exists(data: List[Int], f: (Int) => Boolean): Boolean = {
    if(data.isEmpty) false
    else if(f(data.head)) true
    else exists(data.tail, f)
  }

  def filter(data: List[Int], f: (Int) => Boolean): List[Int] = {
    @tailrec
    def loop(data: List[Int], res: List[Int]): List[Int] = {
      if(data.isEmpty) res
      else{
        if(f(data.head))
          loop(data.tail, res :+ data.head)
        else
          loop(data.tail, res)
      }
    }
    loop(data, List())
  }

  @tailrec
  def forall(data: List[Int], f: (Int) => Boolean): Boolean = {
    if(data.isEmpty) true
    else if(!f(data.head)) false
    else forall(data.tail, f)

  }

  def pascal(c: Int, r: Int): Int = {
    if(c == 1 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

}
