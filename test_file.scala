object main {
  def main(args: Array[String]): Unit = {
    val arr = List(5, 10, 15, 20)
    println(sumList(arr))
  }
  
  def sumList(l: List[Int]): Int = {
    var sum = 0
    for(x <- l){
      sum += x
    }
    sum
  }
}
