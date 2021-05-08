import java.awt.Color

import Manipulation.{Coords, Section}
import Utils.{getCoords, rotate90degCoords, rotateCoords90Left, rotateCoords90Right}

import scala.util.Random

trait QTree[+A]

object QTree{


  /**********************************************************************
   *
   *                 TASK 4 - Rotation Effect (tested and working)
   *
   * *********************************************************************/

  /*
  *  Rotate the given tree 90 degrees to the right
  * */
  def rotate90DegreesRight(t:QTree[Coords]): QTree[Coords] = {
    val c = rotate90degCoords(t)
    subRotate90DegreesRight(t,c,c._2._2)
  }

  def subRotate90DegreesRight(t:QTree[Coords], coords: Coords, imgWidth: Int): QTree[Coords] = {
    t match {
      case QEmpty => QEmpty
      case QLeaf((c, color)) => QLeaf((coords,color))
      case QNode(c, fi, se, th, fo) =>
        QNode(coords,
          subRotate90DegreesRight(th,rotateCoords90Right(getCoords(th),imgWidth),imgWidth),
          subRotate90DegreesRight(fi,rotateCoords90Right(getCoords(fi),imgWidth),imgWidth),
          subRotate90DegreesRight(fo,rotateCoords90Right(getCoords(fo),imgWidth),imgWidth),
          subRotate90DegreesRight(se,rotateCoords90Right(getCoords(se),imgWidth),imgWidth))
    }
  }

  /*
  *  Rotate the given tree 90 degrees to the left
  * */
  def rotate90DegreesLeft(t:QTree[Coords]): QTree[Coords] = {
    val c = rotate90degCoords(t)
    subRotate90DegreesLeft(t,c,c._2._1)
  }

  def subRotate90DegreesLeft(t:QTree[Coords], coords: Coords, imgHeight: Int): QTree[Coords] = {
    t match {
      case QEmpty => QEmpty
      case QLeaf((c, color)) => QLeaf((coords,color))
      case QNode(c, fi, se, th, fo) =>
        QNode(coords,
          subRotate90DegreesLeft(se,rotateCoords90Left(getCoords(se),imgHeight),imgHeight),
          subRotate90DegreesLeft(fo,rotateCoords90Left(getCoords(fo),imgHeight),imgHeight),
          subRotate90DegreesLeft(fi,rotateCoords90Left(getCoords(fi),imgHeight),imgHeight),
          subRotate90DegreesLeft(th,rotateCoords90Left(getCoords(th),imgHeight),imgHeight))

    }
  }

  /**********************************************************************
   *
   *                 TASK 2 - Scale Effect
   *
   * *********************************************************************/


  //Task 2 -> Método principal: aumenta ou reduz uma imagem segundo um factor decimal(0.0 < factor <= 2.0)
  def scale(factor: Double, qt: QTree[Coords]): QTree[Coords] = {

    if (factor > 1.0)
      scaleUp(factor,qt)
    else if(factor < 1.0)
      scaleDown(factor, qt)
    else
      qt
  }

  //Método auxiliar -> Aumenta o tamanho da imagem segundo um fator decimal ( 1.0 < factor <= 2.0)
  def scaleUp(factor: Double, qt: QTree[Coords]): QTree[Coords] = {

    require(factor > 1 && factor <= 2)

    qt match {
      case QEmpty => QEmpty
      case QLeaf( st: Section) => st match {
        case (coords,color)  => QLeaf((coords._1._1 * factor.toInt, coords._1._2 * factor.toInt),
          (coords._2._1 * factor.toInt +1, coords._2._2 * factor.toInt +1),color)
      }
      case QNode(c,fi,se,th,fo) => QNode(((c._1._1, c._1._2),
        (c._2._1 * factor.toInt + 1, c._2._2 * factor.toInt + 1)),
        scaleUp(factor,fi),scaleUp(factor,se),scaleUp(factor,th),scaleUp(factor,fo))

    }
  }
  //Método auxiliar -> Reduz o tamanho da imagem segundo um fator decimal ( 0.0 < factor < 1.0)
  def scaleDown (factor: Double, qt: QTree[Coords]): QTree[Coords] = {

    require(factor > 0.0 && factor < 1.0)

    qt match {
      case QEmpty => QEmpty
      case QLeaf((( a: Int, b: Int),(c: Int,d: Int), color )) =>
        //Tentativas infrutíferas de definir condição para fazer o blend de cores
        /*if((a == c-1) && (b == d-1)) { println("if")
          QLeaf((a,b),(a,b),color)
        } else { */QLeaf((( a * factor).toInt, (b * factor).toInt),
        ((c * factor).toInt, (d * factor).toInt),color)
      //}
      case QNode(value, first, second, third, fourth) =>
        QNode(((value._1._1, value._1._2),
          ((value._2._1 * factor).toInt, (value._2._2 * factor).toInt)), scaleDown(factor,first),
          scaleDown(factor,second),scaleDown(factor,third),scaleDown(factor,fourth))

    }

  }

  /**********************************************************************
   *
   *                 TASK 2 - Mirror Effect
   *
   * *********************************************************************/

  /*
  *  Mirror the given tree vertically
  * */
  def mirrorV(tree:QTree[Coords]) : QTree[Coords] = {
      subMirrorV(tree, Utils.getRootCoords(tree)._2._1)
  }



  def subMirrorV(tree: QTree[Coords], lastRow: Int): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QLeaf((c: Coords, color)) =>
        val newCoords = ((lastRow - c._2._1,c._1._2),(lastRow - c._1._1, c._2._2))
        QLeaf((newCoords,color))
      case QNode(c, fi, se, th, fo) =>
        val newCoords = ((lastRow - c._2._1,c._1._2),(lastRow - c._1._1, c._2._2))
        QNode(newCoords,
          subMirrorV(th, lastRow),
          subMirrorV(fo, lastRow),
          subMirrorV(fi, lastRow),
          subMirrorV(se, lastRow))

    }
  }

  /*
  *  Mirror the given tree horizontally
  * */
  def mirrorH(tree:QTree[Coords]) : QTree[Coords] = {
    subMirrorH(tree, Utils.getRootCoords(tree)._2._2)
  }

  def subMirrorH(tree: QTree[Coords], lastCol: Int): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QLeaf((c: Coords, color)) =>
        val newCoords = ((c._1._1,lastCol - c._2._2),(c._2._1,lastCol - c._1._2))
        QLeaf((newCoords,color))
      case QNode(c, fi, se, th, fo) =>
        val newCoords = ((c._1._1,lastCol - c._2._2),(c._2._1,lastCol - c._1._2))
        QNode(newCoords,
          subMirrorH(se, lastCol),
          subMirrorH(fi, lastCol),
          subMirrorH(fo, lastCol),
          subMirrorH(th, lastCol))
    }
  }


  /**********************************************************************
   *
   *                 TASK 5 - Map Effects
   *
   * *********************************************************************/


  /*
   *  Pure noise function
   *  */
  def noisePure(color: Color, randNumber: Random): Color = {
    //val rnd = new scala.util.Random
    //val randNumber = ((0 + rnd.nextInt( (100 - 0) + 1 )).toDouble )/100 // Must be between 0 and 255

    new Color((color.getRed() * randNumber.nextDouble()).toInt,
      (color.getGreen() * randNumber.nextDouble()).toInt,
      (color.getBlue() * randNumber.nextDouble()).toInt)
  }

  def mapNoisePure(tree:QTree[Coords], tutu: Int):QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QLeaf((c, color : Color)) => QLeaf((c,noisePure(color, new Random(tutu))))
      case QNode(c, fi, se, th, fo) => QNode(c,
        mapNoisePure(fi, tutu),
        mapNoisePure(se, tutu),
        mapNoisePure(th, tutu),
        mapNoisePure(fo, tutu))
        /*mapNoisePure(fi, new Random(tutu).nextInt()),
        mapNoisePure(se, new Random(tutu).nextInt()),
        mapNoisePure(th, new Random(tutu).nextInt()),
        mapNoisePure(fo, new Random(tutu).nextInt()))  */
    }
  }

  /*
   *  Impure noise function
   *  */
  def noise(color: Color): Color = {
    val rnd = new scala.util.Random
    val randNumber = ((0 + rnd.nextInt( (100 - 0) + 1 )).toDouble )/100 // Must be between 0 and 255

    new Color((color.getRed()   * randNumber).toInt,
              (color.getGreen() * randNumber).toInt,
              (color.getBlue()  * randNumber).toInt)
  }

  def mapColourEffect(f:Color => Color, tree:QTree[Coords]):QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QLeaf((c, color : Color)) => QLeaf((c,f(color)))
      case QNode(c, fi, se, th, fo) => QNode(c,
        mapColourEffect(f,fi),
        mapColourEffect(f,se),
        mapColourEffect(f,th),
        mapColourEffect(f,fo))
    }
  }



  //Efeito sepia
  def sepia(color: Color): Color = {
    //Calcular os novos componentes RGB segundo a fórmula. Nenhuma componente pode ser superior a 255.
    val newRed = if((color.getRed * 0.393 + color.getGreen * 0.769 + color.getBlue * 0.189) > 255)
      255 else (color.getRed * 0.393 + color.getGreen * 0.769 + color.getBlue * 0.189).toInt
    val newGreen = if((color.getRed * 0.349 + color.getGreen * 0.686 + color.getBlue * 0.168) > 255)
      255 else (color.getRed * 0.349 + color.getGreen * 0.686 + color.getBlue * 0.168).toInt
    val newBlue = if((color.getRed * 0.272 + color.getGreen * 0.534 + color.getBlue * 0.131) > 255)
      255 else (color.getRed * 0.272 + color.getGreen * 0.534 + color.getBlue * 0.131).toInt

    color match {
      case null => null
      case color => Color.getColor("",ImageUtil.encodeRgb(newRed,newGreen, newBlue))
    }
  }


  def contrast(color: Color) : Color= {
    // Fator de contraste = 1.2 (20%)
    val newRed = if((1.2*(color.getRed -128) +128)  >255) 255
    else if (1.2*(color.getRed -128) +128 <0) 0
    else (1.2*(color.getRed -128) +128)
    val newGreen = if((1.2*(color.getGreen -128) +128) >255) 255
    else if (1.2*(color.getGreen -128) +128 < 0) 0
    else (1.2*(color.getGreen -128) +128)
    val newBlue= if((1.2*(color.getBlue -128) +128)  >255) 255
    else if ((1.2*(color.getBlue -128) +128) < 0) 0
    else (1.2*(color.getBlue -128) +128)
    color match {
      case null => null
      case color => Color.getColor("",ImageUtil.encodeRgb(newRed.toInt, newGreen.toInt, newBlue.toInt))
    }
  }



}
