import java.awt.Color

import BitMap.{isColorStain, rawQuad1, rawQuad2, rawQuad3, rawQuad4, sliceList, trueQ1, trueQ2, trueQ3, trueQ4, updateMatrix, verticesCoordinates}
import QLeaf.getLeafColor
import QTree.{getQuad, getRootCoords, getType}

object Manipulation {

  type Point = (Int, Int)
  type Coords = (Point, Point)
  type Section = (Coords, Color)

  /**********************************************************************
   *
   *                 TASK 1.1 - BitMap -> QTree (tested and working)
   *
   * *********************************************************************/

  def makeQTree(bitMap: BitMap[Int]): QTree[Coords] = {
    subMakeQTree(bitMap.matrix, verticesCoordinates(bitMap.matrix))
  }


  def subMakeQTree(matrix:List[List[Int]], coords: Coords): QTree[Coords] = {

    if(matrix.size == 0 || matrix.head.size == 0) {  // If empty list
      QEmpty
    } else {
      if(isColorStain(matrix,coords)) {  // If it is the same color in all quadrant
        QLeaf((coords, new Color(matrix.head.head)))
      } else {

        // sliceList(matrix)(rawQuad) - Slice list in the respective quadrant
        // trueQ(coords,matrix)       - Calculates the quadrant coordinates in
        //                              relation to the parent image

        QNode(coords,
          subMakeQTree(sliceList(matrix)(rawQuad1),trueQ1(coords, matrix)),
          subMakeQTree(sliceList(matrix)(rawQuad2),trueQ2(coords, matrix)),
          subMakeQTree(sliceList(matrix)(rawQuad3),trueQ3(coords, matrix)),
          subMakeQTree(sliceList(matrix)(rawQuad4),trueQ4(coords, matrix)),
        )
      }
    }
  }

  /**********************************************************************
   *
   *                 TASK 1.2 - QTree -> BitMap (tested and working)
   *
   * *********************************************************************/

  def makeBitMap(tree: QTree[Coords]): List[List[Int]] = {
    val imgCoords = getRootCoords(tree)
    val newList = List.fill(imgCoords._2._1 + 1)(List())
    subMakeBitMap(tree, newList)
  }

  def subMakeBitMap(t: QTree[Coords], list:List[List[Int]]): List[List[Int]] = {
    val type_ = getType(t)

    type_ match{

      case "QEmpty" => list

      case "QLeaf" =>

        val c = getRootCoords(t)
        updateMatrix(c,
          ImageUtil.encodeRgb(
            getLeafColor(t).getRed(),
            getLeafColor(t).getGreen(),
            getLeafColor(t).getBlue()), c._2._1, list)

      case "QNode" =>

        // Must fill the matrix starting by the 1st quadrant,
        // then 2nd, 3rd and 4th by this specific order
        // Once it got to the fourth (last one) it can return

        val a = subMakeBitMap(getQuad(t,1),list)
        val b = subMakeBitMap(getQuad(t,2),a)
        val c = subMakeBitMap(getQuad(t,3),b)
        subMakeBitMap(getQuad(t,4),c)

    }
  }

}
