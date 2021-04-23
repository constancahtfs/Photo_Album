import Manipulation.{Coords, Section}
import Utils.{getCoords, rotate90degCoords, rotateCoords90Left, rotateCoords90Right}

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
    subMirrorV(tree, Utils.getRootCoords(tree))
  }

  def subMirrorV(tree: QTree[Coords], coordsTemp: Coords): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QLeaf((c, color)) => QLeaf((coordsTemp,color))
      case QNode(c,fi,se,th,fo) => QNode(coordsTemp,
        subMirrorV(th, Utils.trueQuad1(coordsTemp)),
        subMirrorV(fo, Utils.trueQuad2(coordsTemp)),
        subMirrorV(fi, Utils.trueQuad3(coordsTemp)),
        subMirrorV(se, Utils.trueQuad4(coordsTemp)))

    }
  }

  /*
  *  Mirror the given tree horizontally
  * */
  def mirrorH(tree:QTree[Coords]) : QTree[Coords] = {
    subMirrorH(tree, Utils.getRootCoords(tree))
  }

  def subMirrorH(tree: QTree[Coords], coordsTemp: Coords): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QLeaf((c, color)) => QLeaf((coordsTemp,color))
      case QNode(c, fi, se, th, fo) => QNode(coordsTemp,
        subMirrorH(se, Utils.trueQuad1(coordsTemp)),
        subMirrorH(fi, Utils.trueQuad2(coordsTemp)),
        subMirrorH(fo, Utils.trueQuad3(coordsTemp)),
        subMirrorH(th, Utils.trueQuad4(coordsTemp)))
    }
  }

}
