import java.awt.Color

import Manipulation.{Coords, Section}

case class QLeaf[A, B](value: B) extends QTree[A]

object QLeaf{
  /*
  * Gets the color of the given leaf
  *
  * */
  def getLeafColor(leaf: QTree[Coords]): Color = {
    leaf match {
      case QLeaf(section: Section) =>
        section match {
          case (coords, color) => color
        }
      case QLeaf(((a: Int,b: Int),(c: Int,d: Int),color: Color)) => color
    }
  }
}