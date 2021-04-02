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

  // Recebe um array multidimensional (matriz) de cores
  // verificar se a cor e a mesma de um vertice ao outro
  // se sim -> folha + cor
  // se nao -> dividir em 4 e repetir o processo p cada quadrante
  //def makeQTree(b:BitMap[Int]): QTree[] = {}

  // Testing
  def main(args: Array[String]): Unit = {

    // Path da imagem
    val path = "C:\\Users\\const\\IdeaProjects\\Photo_Album\\src\\rgbColors.png";

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)

    // Create bitmap and print it
    val bitMap = BitMap(converted)
    println(bitMap)

  }
}
