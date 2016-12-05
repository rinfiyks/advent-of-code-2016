package util

class Point(private var x: Int, private var y: Int) {

  def getX: Int = x
  def setX(x: Int) = this.x = x
  def getY: Int = y
  def setY(y: Int) = this.y = y

  override def equals(o: Any) = o match {
    case p: Point => p.getX == x && p.getY == y
    case _ => false
  }

}
