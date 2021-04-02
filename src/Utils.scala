import Manipulation.Coords

object Utils {

  // get vertices coordinates of a bitmap
  def verticesCoordinates[A](bitMap: BitMap[A]): Coords ={
    bitMap.matrix match{
      case List() => ((0,0),(0,0))
      case _ => ((0,0),(bitMap.matrix.head.size,bitMap.matrix.size))
    }
  }

  def toListOfLists(l: List[Array[Int]]): List[List[Int]] = {
    l match{
      case List() => List()
      case (h::t) => h.toList :: toListOfLists(t)
    }
  }


}
