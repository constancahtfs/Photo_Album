import java.io.File

import BitMap.toListOfLists
import IO_Utils.{getUserInput, showPrompt, showSubmenu, subLoop}
import Manipulation.makeQTree

import scala.annotation.tailrec

object Main {

  /*************************************************************************************************************
   *
   *    -- This part is meant to run each task and test it with all the avaiable images
   *    inside this project with the textual menu
   *
   *    -- For now we have made and tested completely as shown bellow the following tasks:
   *          - Task 1 - MakeQTree             (declared in QTree)
   *          - Task 1 - MakeBitMap            (declared in QTree)
   *          - Task 2 - Scale Up              (declares in QTree)
   *          - Task 3 - Mirror Vertically     (declared in QTree)
   *          - Task 3 - Mirror Horizontally   (declared in QTree)
   *          - Task 4 - Rotate90DegreesRight  (declared in QTree)
   *          - Task 4 - Rotate90DegreesLeft   (declared in QTree)
   *          - Task 5 - Sepia                 (declared in QTree)
   *          - Task 5 - Impure Noise          (declared in QTree)
   *          - Task 5 - Pure Noise            (declared in QTree)
   *          - Task 5 - Contrast              (declared in QTree)
   *
   *    -- We have come across some problems with the following:
   *          - Task 2 - Scale Down            (declared in QTree)
   *
   *
   * ************************************************************************************************************/


  // Show user the available images
  def presentation(l: List[File]): Unit = {
    l match {
      case Nil => println("Empty List")
      case List(x) => println(x.getName)
      case x :: xs => println(x.getName) :: List(presentation(xs))
    }
  }

  def main(args: Array[String]): Unit = {

    val folder = new File("src/Images")
    val listOfFiles = folder.listFiles().toList
    println("Images available: ")
    presentation(listOfFiles)
    println()
    print("Write the name of the image (including extension) you want to operate: ")
    val imageFile = new File("src/Images/" + getUserInput())
    val path = imageFile.getAbsolutePath
    mainLoop()

    @tailrec
    def mainLoop() {

      showPrompt()
      val userInput = getUserInput()

      // handle the result
      userInput match {
        case "1" => {
          val bitmap = BitMap(toListOfLists(ImageUtil.readColorImage(path).toList))
          val tree = makeQTree(BitMap(toListOfLists(ImageUtil.readColorImage(path).toList)))
          println("Bitmap original: " + bitmap)
          println("Árvore original: " + tree)
          showSubmenu()
          subLoop(tree)
          mainLoop()
        }
        case _ => {
          print("Bye!")
          // return out of the recursion here
        }
      }
    }

  }
}
