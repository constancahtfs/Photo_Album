import Manipulation.{makeBitMap, makeQTree}

object Main {

  def task1(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: " + converted)

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    var tree = makeQTree(bitMap);
    println("[Tree] a partir do bitmap: " + tree)

    println("\n **** QTree -> Bitmap *** \n")

    val bitMap2 = makeBitMap(tree)
    println("[BitMap] a partir da árvore: " + bitMap2)
  }

  def task2(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: " + converted)

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    var tree = makeQTree(bitMap);
    println("[Tree] original: " + tree)

    println("\n **** Espelhamento da Árvore *** \n")

    var mirrorImgH = Effects.mirrorH(tree)
    var mirrorImgV = Effects.mirrorV(tree)
    println("[Tree] Espelho Horizontal: " + mirrorImgH)
    println("[Tree] Espelho Vertical: " + mirrorImgV)

    println("\n **** Espelhamento da Árvore -> Bitmap *** \n")

    val bitMap4 = makeBitMap(mirrorImgH)
    val bitMap5 = makeBitMap(mirrorImgV)
    println("[BitMap] Espelho Horizontal: " + bitMap4)
    println("[BitMap] Espelho Vertical: " + bitMap5)
  }

  def task4(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: " + converted)

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    var tree = makeQTree(bitMap);
    println("[Tree] original: " + tree)

    println("\n **** Rotações da Árvore *** \n")

    var rotatedImg = Effects.rotate90DegreesRight(tree, Utils.verticesCoordinates(bitMap.matrix))
    var rotatedImg2 = Effects.rotate90DegreesLeft(tree, Utils.verticesCoordinates(bitMap.matrix))
    println("[Tree] 90 direita: " + rotatedImg)
    println("[Tree] 90 esquerda: " + rotatedImg2)

    println("\n **** Rotações da Árvore -> BitMap *** \n")

    val bitMap2 = makeBitMap(rotatedImg)
    val bitMap3 = makeBitMap(rotatedImg2)
    println("[BitMap] 90 direita: " + bitMap2)
    println("[BitMap] 90 esquerda: " + bitMap3)

  }

  // Testing
  def main(args: Array[String]): Unit = {

    var img3by3 = "C:\\Users\\const\\IdeaProjects\\Photo_Album\\src\\3by3.png"
    var img4by4 = "C:\\Users\\const\\IdeaProjects\\Photo_Album\\src\\4by4.png"

    task2(img4by4)
    task4(img4by4)
  }
}
