import Manipulation.{Coords, makeBitMap}
import QTree._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object IO_Utils {

  //Constantes usadas em vários métodos
  val exitSubMenu = "0) Exit this submenu"
  val exitedSubMenu = "Exited submenu"
  val bitMapOriginal = "BitMap original: "
  val arvoreOriginal = "Árvore original: "
  val bitMapTransformado = "BitMap transformado: "
  val arvoreTransformada = "Árvore transformada: "

  //Mostrar as opções ao utilizador
  def showPrompt(): Unit = {

    println("Choose one of the options below: ")
    println("1) Convert the image you chose to a tree to perform more operations ")
    println("0) To exit")

  }

  //Mostrar as várias operações para a tree
  def showSubmenu(): Unit = {
    println("1) Scale an image as a tree ")
    println("2) Mirror an image as a tree ")
    println("3) Rotate an image as a tree ")
    println("4) Apply effects to an image as a tree ")
    println(exitSubMenu)

  }
  //Mostrar as opções para o método scale
  def showSubPromptScale(): Unit = {
    println("1) Scale up or down a tree")
    println(exitSubMenu)

  }
  //Mostrar as opções para o método mirror
  def showSubPromptMirror(): Unit = {
    println("1) Mirror horizontally a tree")
    println("2) Mirror vertically a tree")
    println(exitSubMenu)

  }
  //Mostrar as opções para o método rotate
  def showSubPromptRotate(): Unit = {
    println("1) Rotate a tree to the left")
    println("2) Rotate a tree to the right")
    println(exitSubMenu)

  }

  def showSubPromptEffects(): Unit = {
    println("1) Apply impure noise effect")
    println("2) Apply pure noise effect")
    println("3) Apply contrast effect")
    println("4) Apply sepia effect")
    println(exitSubMenu)

  }
  //Receber o input do utilizador
  //def getUserInput(s: String): Try[Int] = Try(s.trim.toInt)
  def getUserInput(): String = readLine().trim

  //Submenu para aplicar método scale
  @tailrec
  def subLoopScale(t: QTree[Coords]): Unit ={
    showSubPromptScale()
    val subInputUser = getUserInput()
    subInputUser match {
      case "1" => {
        println("Insira o factor entre 0.0 (exclusivé) e 2.0 (inclusivé) para mudar a dimensão da árvore: ")
        val factor = getUserInput().toDouble
        println(arvoreTransformada + scale(factor, t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(scale(factor,t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopScale(t)
      }
      case _ => println(exitedSubMenu)
    }
  }
  //Submenu para aplicar o método mirror
  @tailrec
  def subLoopMirror(t: QTree[Coords]): Unit = {
    showSubPromptMirror()
    val subInputUser = getUserInput()
    subInputUser match {
      case "1" => {
        println(t)
        println(arvoreTransformada + mirrorH(t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(mirrorH(t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopMirror(t)
      }
      case "2" => {
        println(arvoreTransformada + mirrorV(t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(mirrorV(t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopMirror(t)
      }
      case _ => println(exitedSubMenu)
    }
  }
  //Submenu para aplicar método rotate
  @tailrec
  def subLoopRotate(t: QTree[Coords]): Unit = {
    showSubPromptRotate()
    val subInputUser = getUserInput()
    subInputUser match {
      case "1" => {
        println(arvoreTransformada + rotate90DegreesLeft(t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(rotate90DegreesLeft(t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopRotate(t)
      }
      case "2" => {
        println(arvoreTransformada + rotate90DegreesRight(t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(rotate90DegreesRight(t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopRotate(t)
      }
      case _ => println(exitedSubMenu)
    }
  }
  @tailrec
  def subLoopEffects(t: QTree[Coords]): Unit = {
    showSubPromptEffects()
    val subInputUser = getUserInput()
    subInputUser match {
      case "1" => {
        println(arvoreTransformada + mapColourEffect(noise,t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(mapColourEffect(noise, t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopEffects(t)
      }
      case "2" => {
        println(arvoreTransformada + mapNoisePure(t,10))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(mapNoisePure(t,10)._1))
        println(bitMapOriginal + makeBitMap(t))
        subLoopEffects(t)
      }
      case "3" => {
        println(arvoreTransformada + mapColourEffect(contrast,t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(mapColourEffect(contrast, t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopEffects(t)
      }
      case "4" => {
        println(mapColourEffect(sepia,t))
        println(arvoreOriginal + t)
        println(bitMapTransformado + makeBitMap(mapColourEffect(sepia, t)))
        println(bitMapOriginal + makeBitMap(t))
        subLoopEffects(t)
      }
      case _ => println(exitedSubMenu)
    }
  }

  def subLoop(t: QTree[Coords]): Unit = {
    val subInputUser = getUserInput()
    subInputUser match {
      case "1" => subLoopScale(t)
      case "2" => subLoopMirror(t)
      case "3" => subLoopRotate(t)
      case "4" =>  subLoopEffects(t)
      case _ => println("Exit submenu")
    }
  }
}