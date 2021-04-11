import java.awt.Color

object Manipulation {

  // Important types
  type Point = (Int, Int)
  type Coords = (Point, Point)
  type Section = (Coords, Color)

  def makeQTree(bitMap: BitMap[Int]): QTree[Coords] = {
    subMakeQTree(bitMap.matrix, Utils.verticesCoordinates(bitMap.matrix))
  }

  def subMakeQTree(matrix:List[List[Int]], coords: Coords): QTree[Coords] = {

    if(matrix.size == 0 || matrix.head.size == 0)  // If List()
      QEmpty
    else
      if(Utils.isColorStain(matrix,coords))  // If it is the same color in all quadrant
        QLeaf((coords, new Color(matrix.head.head)))
      else
        QNode(coords,
          subMakeQTree(Utils.sliceList(matrix)(Utils.rawQuad1),Utils.trueQuad1(coords)),
          subMakeQTree(Utils.sliceList(matrix)(Utils.rawQuad2),Utils.trueQuad2(coords)),
          subMakeQTree(Utils.sliceList(matrix)(Utils.rawQuad3),Utils.trueQuad3(coords)),
          subMakeQTree(Utils.sliceList(matrix)(Utils.rawQuad4),Utils.trueQuad4(coords)),
        )
  }

  def makeBitMap(tree: QTree[Coords]): List[List[Int]] = {
    val imgCoords = Utils.getRootCoords(tree)
    val newList = List.fill(imgCoords._2._1 + 1)(List())
    subMakeBitMap(tree, newList)
  }

  def subMakeBitMap(t: QTree[Coords], list:List[List[Int]]): List[List[Int]] = {
    val type_ = Utils.getType(t)
    type_ match{
      case "QLeaf" =>
        val c = Utils.getRootCoords(t)
        Utils.updateMatrix(c,
          ImageUtil.encodeRgb(
            Utils.getLeafColor(t).getRed(),
            Utils.getLeafColor(t).getGreen(),
            Utils.getLeafColor(t).getBlue()), c._2._1, list)
      case "QNode" =>
        val ab = Utils.getRootCoords(t)
        val a = subMakeBitMap(Utils.getQuad1(t),list)
        val b = subMakeBitMap(Utils.getQuad2(t),a)
        val c = subMakeBitMap(Utils.getQuad3(t),b)
        subMakeBitMap(Utils.getQuad4(t),c)
      case "QEmpty" => list
    }
  }

}
