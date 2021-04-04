import Manipulation.Coords

object Utils {

  // get vertices coordinates of a matrix
  def verticesCoordinates[A](matrix:List[List[A]]): Coords ={
    matrix match{
      case List() => ((0,0),(0,0))
      case _ => ((0,0),(even(matrix.size) - 1, even(matrix.head.size) - 1))
    }
  }

  def even(number: Int): Int = {
    if(number % 2 == 0) // number is even
      number
    else // number is odd
      number + 1 // make it even
  }

  // otimize this

  def trueQuad1(coords: Coords): Coords = {
    val a = coords._1._1
    val b = coords._1._2
    val c = coords._2._1
    val d = coords._2._2
    ((a,b),( a +  ((c-a)-1)/2   , b + ((d-b)+1)/2 - 1)   )
  }

  def trueQuad2(coords: Coords): Coords = {
    val a = coords._1._1
    val b = coords._1._2
    val c = coords._2._1
    val d = coords._2._2
    (  (a,  b + ((d-b)+1)/2   ),( a +  ((c-a)-1)/2   ,   d))
  }

  def trueQuad3(coords: Coords): Coords = {
    val a = coords._1._1
    val b = coords._1._2
    val c = coords._2._1
    val d = coords._2._2
    (  ( a + ((c-a)+1)/2 ,   b   ),(   c   ,  b + ((d-b)-1)/2)  )
  }

  def trueQuad4(coords: Coords): Coords = {
    val a = coords._1._1
    val b = coords._1._2
    val c = coords._2._1
    val d = coords._2._2
    (  (  a + ((c-a)+1)/2 ,  b + ((d-b)+1)/2   ),(   c   ,   d  ))
  }


  def rawQuad1[A](matrix:List[List[A]]): Coords ={
    ((0,0),((even(matrix.size) - 1)/2, (even(matrix.head.size) - 1)/2))
  }

  def rawQuad2[A](matrix:List[List[A]]): Coords ={
    ((0,even(matrix.head.size)/2),((even(matrix.size) - 1)/2, even(matrix.head.size) - 1))
  }

  def rawQuad3[A](matrix:List[List[A]]): Coords ={
    ((even(matrix.size)/2,0),(even(matrix.size) - 1, (even(matrix.head.size) - 1)/2))
  }

  def rawQuad4[A](matrix:List[List[A]]): Coords ={
    ((even(matrix.size)/2,even(matrix.head.size)/2),(even(matrix.size) - 1, even(matrix.head.size) - 1))
  }

  // conversion
  def toListOfLists(l: List[Array[Int]]): List[List[Int]] = {
    l match{
      case List() => List()
      case (h::t) => h.toList :: toListOfLists(t)
    }
  }

  // cuts the given matrix by the given quadrant
  def sliceList[A](l:List[List[A]])(f: List[List[A]] => Coords): List[List[A]] = {
      sliceColumns(f(l),sliceRows(f(l),l))
  }

  // cuts list vertically
  def sliceRows[A](c:Coords, l:List[List[A]]): List[List[A]] = {
    l.drop(c._1._1).take(c._2._1 + 1)
  }


  // cuts list horizontally
  def sliceColumns[A](c:Coords, l:List[List[A]]): List[List[A]] = {
    l match{
      case List() => List()
      case (h::t) => h.take(c._2._2 + 1).drop(c._1._2) :: sliceColumns(c,t)
    }
  }

  // checks if the given list has the same number in all positions
  def sameColor(l: List[List[Int]]): Boolean = {
    l match{
      case List() => true
      case (h:: List()) => subSameColor(h)
      case (h::t) => if(subSameColor(h) && h == t.head) sameColor(t) else false
    }
  }

  def subSameColor(l: List[Int]): Boolean = {
    l match{
      case List() => true
      case (h:: Nil) => true
      case (h::t) => if(h == t.head) subSameColor(t) else false
    }
  }

}
