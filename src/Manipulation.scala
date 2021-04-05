import java.awt.Color

// Tree
trait QTree[+A]

// Node with its coordinates and respective 4 quadrants
case class QNode[A](value: A,
                    one: QTree[A], two: QTree[A],
                    three: QTree[A], four: QTree[A]) extends QTree[A]

// Leaf with coordinates and color
case class QLeaf[A, B](value: B) extends QTree[A]

// Empty tree
case object QEmpty extends QTree[Nothing]


// Bit Map
case class BitMap[A](matrix: List[List[A]])


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
        QLeaf(coords, matrix.head.head)
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

  // Testing
  def main(args: Array[String]): Unit = {
    val bigList = List(List(1,1,1),List(2,2,2),List(3,3,3))

    // Path da imagem
    val path = "C:\\Users\\const\\IdeaProjects\\Photo_Album\\src\\3by3.png";

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)
    println(converted)

    // Create bitmap and print it
    val bitMap = BitMap(converted)
    var tree = makeQTree(bitMap);
    println(tree)


    //val samecolor = Utils.sameColor(quad4)
    // println(samecolor)

    //val l = List(-16735512,-20791,-14503604,-32985)
    //val l = List(-12629812,-3947581,-4621737)
    //printColors(l)



  }
}
