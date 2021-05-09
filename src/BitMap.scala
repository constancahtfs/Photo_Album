import Manipulation.Coords
import Utils.{assertCols, assertRows, even}

import scala.annotation.tailrec

case class BitMap[A](matrix: List[List[A]])

object BitMap{

  /*
  *  Gets vertices coordinates of the given matrix
  *
  * */
  def verticesCoordinates[A](matrix:List[List[A]]): Coords ={
    matrix match{
      case List() => ((0,0),(0,0))
      case _ => ((0,0),(matrix.size - 1, matrix.head.size - 1))
    }
  }

  /*
  *  Will update the given matrix with the given color in the given coordinates space
  *  The counter will indicate the stopping moment
  *
  * */
  def updateMatrix(coords: Coords, color: Int, counter: Int, list:List[List[Int]]): List[List[Int]] = {

    if(counter >= list.size)
      list
    else
      if(counter != (coords._1._1 - 1)) {
        val newMatrix = matrixAppend(counter, coords._2._2 - coords._1._2 + 1, color, list)
        updateMatrix(coords, color, counter - 1, newMatrix)
      }
      else
        list
  }

  /*
  *  Appends a list filled with the given value (color) and concatenates it with
  * the matrix
  *
  * */
  def matrixAppend(line:Int, width:Int,value:Int, list:List[List[Int]]): List[List[Int]] = {
    list.updated(line,list(line) ::: List.fill(width)(value))
  }

  /*
  *  The next 4 definitions calculate the coordinates of each quadrant
  *  If the image has odd sizes, the definitions will make it even and create
  * illusory coordinates
  *
  * */

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

  /*
  *  Conversion from list of arrays to list of lists in order to
  * preserve the immutability
  *
  * */
  def toListOfLists(l: List[Array[Int]]): List[List[Int]] = {
    l match{
      case List() => List()
      case (h::t) => h.toList :: toListOfLists(t)
    }
  }

  /*
  *  Cuts list by range of columns and rows
  *  That range is calculated by the function in the parameters
  *  This function indicates in which quadrant the list has to be cut
  *
  * */
  def sliceList[A](l:List[List[A]])(f: List[List[A]] => Coords): List[List[A]] = {
    sliceColumns(f(l),sliceRows(f(l),l))
  }

  /*
  *  Cuts list vertically
  *
  * */
  def sliceRows[A](c:Coords, l:List[List[A]]): List[List[A]] = {
    l.drop(c._1._1).take(c._2._1 + 1)

  }

  /*
  *  Cuts list horizontally
  *
  * */
  def sliceColumns[A](c:Coords, l:List[List[A]]): List[List[A]] = {
    l match{
      case List() => List()
      case (h::t) => h.take(c._2._2 + 1).drop(c._1._2) :: sliceColumns(c,t)
    }
  }

  /*
  *  Checks if the given list has the same number in all positions
  *
  * */
  @tailrec
  def sameColor(l: List[List[Int]]): Boolean = {
    l match{
      case List() => true
      case (h:: List()) => subSameColor(h)
      case (h::t) => if(subSameColor(h) && h == t.head) sameColor(t) else false
    }
  }
  @tailrec
  def subSameColor(l: List[Int]): Boolean = {
    l match{
      case List() => true
      case (h:: Nil) => true
      case (h::t) => if(h == t.head) subSameColor(t) else false
    }
  }

  /*
  *  Checks if the given list occupies all the space given by the coordinates
  *
  * */
  def occupiesAllSpace[A](l: List[List[A]], c: Coords): Boolean = {

    if(l.size == 1 && l.head.size == 1){
      true
    }
    else{
      val rows = c._2._1 - c._1._1
      val cols = c._2._2 - c._1._2

      if((l.size - 1) == rows && (l.head.size - 1) == cols)
        true
      else
        false
    }
  }

  /*
   *  Checks if the given list has the same color (rgb encoded to an integer) in all
   * its space
   *
   * */
  def isColorStain(l: List[List[Int]], c: Coords): Boolean = {

    if(sameColor(l) && occupiesAllSpace(l, c))
      true
    else
      false
  }

  // The following are the trueQuadX definitions updated to delimit the coordinates
  // in order to accept odd images

  def trueQ1[A](coords: Coords, matrix: List[List[A]]): Coords = {
    val c2 = coords
    val a = c2._1._1
    val b = c2._1._2
    val c = assertRows(a,c2._2._1)
    val d = assertCols(b,c2._2._2)
    val finalRows = a + ((c-a)-1)/2
    val finalCols = b + ((d-b)+1)/2 - 1
    val matrixRows = c2._1._1 + matrix.size - 1
    val matrixCols = c2._1._2 + matrix.head.size - 1

    if(finalRows >= matrixRows)
      if(finalCols >= matrixCols)
        ((a,b),(matrixRows, matrixCols))
      else
        ((a,b),(matrixRows,finalCols))
    else
      if(finalCols >= matrixCols)
        ((a,b),(finalRows, matrixCols))
      else
        ((a,b),(finalRows,finalCols))
  }

  def trueQ2[A](coords: Coords, matrix: List[List[A]]): Coords = {
    val c2 = coords
    val a = c2._1._1
    val b = c2._1._2
    val c = assertRows(a,c2._2._1)
    val d = assertCols(b,c2._2._2)
    val finalRows = a + ((c-a)-1)/2
    val finalCols = d
    val matrixRows = c2._1._1 + matrix.size - 1
    val matrixCols = c2._1._2 + matrix.head.size - 1

    if(finalRows >= matrixRows)
      if(finalCols >= matrixCols)
        ((a,b + ((d-b)+1)/2),(matrixRows, matrixCols))
      else
        ((a,b + ((d-b)+1)/2),(matrixRows,finalCols))
    else
      if(finalCols >= matrixCols)
        ((a,b + ((d-b)+1)/2),(finalRows, matrixCols))
      else
        ((a,b + ((d-b)+1)/2),(finalRows,finalCols))
  }

  def trueQ3[A](coords: Coords, matrix: List[List[A]]): Coords = {
    val c2 = coords
    val a = c2._1._1
    val b = c2._1._2
    val c = assertRows(a,c2._2._1)
    val d = assertCols(b,c2._2._2)
    val finalRows = c
    val finalCols = b + ((d-b)-1)/2
    val matrixRows = c2._1._1 + matrix.size - 1
    val matrixCols = c2._1._2 + matrix.head.size - 1

    if(finalRows >= matrixRows)
      if(finalCols >= matrixCols)
        ((a + ((c-a)+1)/2 ,b),(matrixRows, matrixCols))
      else
        ((a + ((c-a)+1)/2 ,b),(matrixRows,finalCols))
    else
      if(finalCols >= matrixCols)
        ((a + ((c-a)+1)/2 ,b),(finalRows, matrixCols))
      else
        ((a + ((c-a)+1)/2 ,b),(finalRows,finalCols))
  }

  def trueQ4[A](coords: Coords, matrix: List[List[A]]): Coords = {
    val c2 = coords
    val a = c2._1._1
    val b = c2._1._2
    val c = assertRows(a,c2._2._1)
    val d = assertCols(b,c2._2._2)
    val finalRows = c
    val finalCols = d
    val matrixRows = c2._1._1 + matrix.size - 1
    val matrixCols = c2._1._2 + matrix.head.size - 1

    if(finalRows >= matrixRows)
      if(finalCols >= matrixCols)
        ((a + ((c-a)+1)/2 ,b + ((d-b)+1)/2),(matrixRows, matrixCols))
      else
        ((a + ((c-a)+1)/2 ,b + ((d-b)+1)/2),(matrixRows,finalCols))
    else
      if(finalCols >= matrixCols)
        ((a + ((c-a)+1)/2 ,b + ((d-b)+1)/2),(finalRows, matrixCols))
      else
        ((a + ((c-a)+1)/2 ,b + ((d-b)+1)/2),(finalRows,finalCols))
  }

}
