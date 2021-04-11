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


}
