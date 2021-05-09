import Manipulation.Coords



object Utils {

  /*************************************************************************************************************
   *
   *    -- This part contains all the utilitary functions needed in order
   *    to make the main tasks
   *
   * ************************************************************************************************************/

  /**********************************************************************
   *
   *                 General Utils
   *
   * *********************************************************************/



  def even(number: Int): Int = {
    if(number % 2 == 0) // number is even
      number
    else // number is odd
      number + 1 // make it even
  }

  def odd(number: Int): Int = {
    if(number == 0)
      number
    else{
      if(number % 2 == 0) // number is even
        number + 1 // make it odd
      else // number is odd
        number
    }

  }

  def assertRows(a:Int, c:Int): Int = {
    if(((c+1)-a) % 2 == 0)
      c
    else
      c+1
  }

  def assertCols(b:Int, d:Int): Int = {
    if(((d+1)-b) % 2 == 0)
      d
    else
      d+1
  }

  /*
  *  Calculates the given coordinates when they suffer from a 90 degree
  * rotation to the right
  *  The imgWidth refers to the width of the final rotated image
  *
  * */
  def rotateCoords90Right(coords: Coords, imgWidth: Int): Coords = {

    if(coords == ((-1,-1),(-1,-1))) { // Is QEmpty, does not matter
      coords
    } else{
      val y1 = coords._1._1
      val x1 = coords._1._2
      val y2 = coords._2._1
      val x2 = coords._2._2

      ((x1,Math.abs(-y2 + imgWidth)),(x2,Math.abs(-y1 + imgWidth)))
    }
  }

  /*
  *  Calculates the given coordinates when they suffer from a 90 degree
  * rotation to the left
  * The imgHeight refers to the width of the final rotated image
  *
  * */
  def rotateCoords90Left(coords: Coords, imgHeight: Int): Coords = {

    if(coords == ((-1,-1),(-1,-1))) { // Is QEmpty, does not matter
      coords
    } else{

      val a1 = coords._1._1
      val b1 = coords._1._2
      val a2 = coords._2._1
      val b2 = coords._2._2

      ((Math.abs(b2 - imgHeight), a1),(Math.abs(b1 - imgHeight), a2))
    }
  }



}
