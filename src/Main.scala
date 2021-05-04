import Manipulation.{makeBitMap, makeQTree}
import QTree.{mapColourEffect, mirrorH, mirrorV, noise, rotate90DegreesLeft, rotate90DegreesRight, scale}

object Main {

  /*************************************************************************************************************
   *
   *    -- This part is meant to run each task and test it with all the avaiable images
   *    inside this project
   *
   *    -- For now we have made and tested completely as shown bellow the following tasks:
   *          - Task 1 - MakeQTree             (declared in Manipulation)
   *          - Task 1 - MakeBitMap            (declared in Manipulation)
   *          - Task 4 - Rotate90DegreesRight  (declared in Effects)
   *          - Task 4 - Rotate90DegreesLeft   (declared in Effects)
   *
   *    -- We have started the following, but with light testing:
   *          - Task 2 - Scale                 (declared in Effects)
   *          - Task 3 - Mirror                (declared in Effects)
   *
   * ************************************************************************************************************/

  val img2by3 = getClass.getClassLoader.getResource("2by3.png").getPath;
  val img3by3 = getClass.getClassLoader.getResource("3by3.png").getPath;
  val img4by4 = getClass.getClassLoader.getResource("4by4.png").getPath;
  val img4by5 = getClass.getClassLoader.getResource("4by5.png").getPath;
  val img5by4 = getClass.getClassLoader.getResource("5by4.png").getPath;
  val img5by7 = getClass.getClassLoader.getResource("5by7.png").getPath;

  def task1(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: \n" + converted)

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    println("[Tree] a partir do bitmap: \n" + tree)

    println("\n **** QTree -> Bitmap *** \n")

    val bitMap2 = makeBitMap(tree)
    println("[BitMap] original vs [BitMap] a partir da árvore: \n" + converted + "\n" + bitMap2)
  }

  def task2(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: " + converted)

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    println("[Tree] original: " + tree)

    println("\n **** Espelhamento da Árvore *** \n")

    val mirrorImgH = mirrorH(tree)
    val mirrorImgV = mirrorV(tree)
    println("[Tree] Espelho Horizontal: " + mirrorImgH)
    println("[Tree] Espelho Vertical: " + mirrorImgV)

    println("\n **** Espelhamento da Árvore -> Bitmap *** \n")

    val bitMap4 = makeBitMap(mirrorImgH)
    val bitMap5 = makeBitMap(mirrorImgV)
    println("[BitMap] Espelho Horizontal: " + bitMap4)
    println("[BitMap] Espelho Vertical: " + bitMap5)

  }

  def task3(path: String): Unit ={

    val originalTree = makeQTree(BitMap(Utils.toListOfLists(ImageUtil.readColorImage(path).toList)))
    println("Original: " + originalTree)
    val treeScaleUp = scale(2,originalTree)
    println("Scale Up: " + treeScaleUp)
    val treeScaleDown = scale(0.5,treeScaleUp)
    println("Scale Down: " + treeScaleDown)

  }

  def task4(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: \n" + converted + "\n")

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    println("[Tree] original: \n" + tree + "\n")

    println("\n **** Rotações da Árvore *** \n")

    val rotatedImg = rotate90DegreesRight(tree)
    val rotatedImg2 = rotate90DegreesLeft(tree)
    println("[Tree] 90 direita: \n" + rotatedImg + "\n")
    println("[Tree] 90 esquerda: \n" + rotatedImg2 + "\n")

    println("\n **** Rotações da Árvore -> BitMap *** \n")

    val bitMap2 = makeBitMap(rotatedImg)
    val bitMap3 = makeBitMap(rotatedImg2)
    println("[BitMap] original: \n" + converted + "\n")
    println("[BitMap] 90 direita: \n" + bitMap2 + "\n")
    println("[BitMap] 90 esquerda: \n" + bitMap3 + "\n")
  }

  def task5(path: String): Unit ={

    println("\n **** Bitmap -> QTree *** \n")

    // Get color for each pixel
    val imageColors = ImageUtil.readColorImage(path)

    // Convert to list of lists
    var converted = Utils.toListOfLists(imageColors.toList)
    println("[BitMap] original: \n" + converted + "\n")

    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)

    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    println("[Tree] original: \n" + tree + "\n")

    println("\n **** Efeitos *** \n")

    val noiseTree = mapColourEffect(noise,tree)
    println("[Tree] Noise: \n" + noiseTree + "\n")

    println("\n **** Efeitos -> BitMap *** \n")

    val bitMap2 = makeBitMap(noiseTree)
    println("[BitMap] original: \n" + converted + "\n")
    println("[BitMap] Noise: \n" + bitMap2 + "\n")

  }

  def task1Testing()={

    println("\n-- 2 POR 3 --\n")
    task1(img2by3)
    println("\n-- 3 POR 3 --\n")
    task1(img3by3)
    println("\n-- 4 POR 4 --\n")
    task1(img4by4)
    println("\n-- 4 POR 5 --\n")
    task1(img4by5)
    println("\n-- 5 POR 4 --\n")
    task1(img5by4)
    println("\n-- 5 POR 7 --\n")
    task1(img5by7)
  }

  def task2Testing()={

    println("\n-- 4 POR 4 --\n")
    task2(img4by4)

  }

  def task3Testing() = {
    println("\n-- 4 POR 4 --\n")
    task3(img4by4)
  }

  def task4Testing()={

    println("\n-- 2 POR 3 --\n")
    task4(img2by3)
    println("\n-- 3 POR 3 --\n")
    task4(img3by3)
    println("\n-- 4 POR 4 --\n")
    task4(img4by4)
    println("\n-- 4 POR 5 --\n")
    task4(img4by5)
    println("\n-- 5 POR 4 --\n")
    task4(img5by4)
    println("\n-- 5 POR 7 --\n")
    task4(img5by7)
  }

  def task5Testing()={

    println("\n-- 2 POR 3 --\n")
    task5(img2by3)
    println("\n-- 3 POR 3 --\n")
    task5(img3by3)
    println("\n-- 4 POR 4 --\n")
    task5(img4by4)
    println("\n-- 4 POR 5 --\n")
    task5(img4by5)
    println("\n-- 5 POR 4 --\n")
    task5(img5by4)
    println("\n-- 5 POR 7 --\n")
    task5(img5by7)
  }

  def main(args: Array[String]): Unit = {
    //task1Testing()
    //task2Testing()
    //task3Testing()
    //task4Testing()
    task5Testing()

  }
}
