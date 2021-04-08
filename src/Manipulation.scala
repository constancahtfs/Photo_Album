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

  def printColors(l: List[Int])={

    for( x <- l ){
      var rgb = ImageUtil.decodeRgb(x).toList
      println(rgb)
    }
  }

  def makeBitMap(tree: QTree[Coords]): List[List[Int]] = {
    val imgCoords = getRootCoords(tree)
    val newList = List.fill(imgCoords._2._1 + 1)(List())
    subMakeBitMap(tree, newList)
  }


  def subMakeBitMap(t: QTree[Coords], list:List[List[Int]]): List[List[Int]] = {
    val type_ = getType(t)
    type_ match{
      case "QLeaf" =>
        val c = getRootCoords(t)
        println("---QLeaf " + c + " vai dar updateMatrix")
        updateMatrix(c, ImageUtil.encodeRgb(getLeafColor(t).getRed(), getLeafColor(t).getGreen(), getLeafColor(t).getBlue()), c._2._1, list)
      case "QNode" =>
        val ab = getRootCoords(t)
        println("QNode " + ab + " vai dar makebitmap nos 4 quads")
        val a = subMakeBitMap(getQuad1(t),list)
        val b = subMakeBitMap(getQuad2(t),a)
        val c = subMakeBitMap(getQuad3(t),b)
        subMakeBitMap(getQuad4(t),c)
      case "QEmpty" => list
    }
  }



  def getLeafColor(leaf: QTree[Coords]): Color = {
    leaf match {
      case QLeaf(section: Section) =>
        section match {
          case (coords, color) => color
        }
    }
  }

  def getQuad1(tree: QTree[Coords]): QTree[Coords] = {
    tree match {
      case QNode(c,fi,se,th,fo) => fi
    }
  }

  def getQuad2(tree: QTree[Coords]): QTree[Coords] = {
    tree match {
      case QNode(c,fi,se,th,fo) => se
    }
  }

  def getQuad3(tree: QTree[Coords]): QTree[Coords] = {
    tree match {
      case QNode(c,fi,se,th,fo) => th
    }
  }

  def getQuad4(tree: QTree[Coords]): QTree[Coords] = {
    tree match {
      case QNode(c,fi,se,th,fo) => fo
    }
  }

  def getRootCoords(tree: QTree[Coords]): Coords =  {
    tree match {
      case QNode(c,fi,se,th,fo) => c
      case QLeaf(section: Section) =>
        section match {
          case (coords, color) => coords
      }
    }
  }

  def getType(tree: QTree[Coords])={
    tree match {
      case QLeaf(section)  => "QLeaf"
      case QNode(c,fi,se,th,fo) => "QNode"
      case QEmpty => "QEmpty"
      case _ => "Unknown"
    }
  }

  def updateMatrix(coords: Coords, color: Int, counter: Int, list:List[List[Int]]): List[List[Int]] = {
    if(counter != (coords._1._1 - 1)) {
      val newMatrix = matrixAppend(counter, coords._2._2 - coords._1._2 + 1, color, list)
      updateMatrix(coords, color, counter - 1, newMatrix)
    }
    else
      list
  }

  def matrixAppend(line:Int, width:Int,value:Int, list:List[List[Int]]): List[List[Int]] = {
    list.updated(line,list(line) ::: List.fill(width)(value))
  }

  def test4by4() ={
    println(" -- TESTE DE UMA IMAGEM DE 4 POR 4 PIXEIS -- ")
    task1("C:\\Users\\const\\IdeaProjects\\Photo_Album\\src\\4by4.png")
  }

  def test3by3() ={
    println(" -- TESTE DE UMA IMAGEM DE 3 POR 3 PIXEIS -- ")
    task1("C:\\Users\\const\\IdeaProjects\\Photo_Album\\src\\3by3.png")
  }

  def task1(path: String): Unit ={

    println(" **** Bitmap -> QTree *** ")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)
    println(converted)

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    var tree = makeQTree(bitMap);
    println(tree)

    println(" **** QTree -> Bitmap *** ")

    val bitMap2 = makeBitMap(tree)
    println(bitMap2)
  }

  // Testing
  def main(args: Array[String]): Unit = {

    test4by4()

  }
}
