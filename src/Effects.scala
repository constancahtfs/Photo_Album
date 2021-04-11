import Manipulation.Coords

object Effects {

  def rotate90DegreesRight(t:QTree[Coords], coords: Coords): QTree[Coords] = {
    t match {
      case QLeaf((c, color)) => QLeaf((coords,color))
      case QEmpty => QEmpty
      case QNode(c, fi, se, th, fo) =>
        QNode(coords,
          rotate90DegreesRight(th,Utils.trueQuad1(coords)),
          rotate90DegreesRight(fi,Utils.trueQuad2(coords)),
          rotate90DegreesRight(fo,Utils.trueQuad3(coords)),
          rotate90DegreesRight(se,Utils.trueQuad4(coords)))
    }
  }

  def rotate90DegreesLeft(t:QTree[Coords], coords: Coords): QTree[Coords] = {
    t match {
      case QLeaf((c, color)) => QLeaf((coords,color))
      case QEmpty => QEmpty
      case QNode(c, fi, se, th, fo) =>
        QNode(coords,
          rotate90DegreesLeft(se,Utils.trueQuad1(coords)),
          rotate90DegreesLeft(fo,Utils.trueQuad2(coords)),
          rotate90DegreesLeft(fi,Utils.trueQuad3(coords)),
          rotate90DegreesLeft(th,Utils.trueQuad4(coords)))

    }
  }

  def mirrorV(tree:QTree[Coords]) : QTree[Coords] = {
    subMirrorV(tree, Utils.getRootCoords(tree))
  }

  def subMirrorV(tree: QTree[Coords], coordsTemp: Coords): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QNode(c,fi,se,th,fo) => QNode(coordsTemp,
        subMirrorV(th, Utils.trueQuad1(coordsTemp)),
        subMirrorV(fo, Utils.trueQuad2(coordsTemp)),
        subMirrorV(fi, Utils.trueQuad3(coordsTemp)),
        subMirrorV(se, Utils.trueQuad4(coordsTemp)))
      case QLeaf((c, color)) => QLeaf((coordsTemp,color))
    }
  }

  def mirrorH(tree:QTree[Coords]) : QTree[Coords] = {
    subMirrorH(tree, Utils.getRootCoords(tree))
  }

  def subMirrorH(tree: QTree[Coords], coordsTemp: Coords): QTree[Coords] = {
    tree match {
      case QEmpty => QEmpty
      case QNode(c, fi, se, th, fo) => QNode(coordsTemp,
        subMirrorH(se, Utils.trueQuad1(coordsTemp)),
        subMirrorH(fi, Utils.trueQuad2(coordsTemp)),
        subMirrorH(fo, Utils.trueQuad3(coordsTemp)),
        subMirrorH(th, Utils.trueQuad4(coordsTemp)))
      case QLeaf((c, color)) => QLeaf((coordsTemp,color))
    }
  }


}
