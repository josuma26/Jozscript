import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class G[T](nodes: List[T], neighbors: Map[T, List[T]]) {
  def path(from: T, to: T): Option[List[T]] = {
    val stack = mutable.Stack[T](from)
    val paths = mutable.Map[T, ListBuffer[T]]()

    while (stack.nonEmpty) {
      val first = stack.pop()
      val pathTo = paths.getOrElseUpdate(first,  ListBuffer[T](first))
      if (first.equals(to)) {
        return Some(pathTo.toList.reverse)
      } else {
        neighbors(first).foreach(neigh => {
          val pathToNeigh = paths.getOrElseUpdate(neigh, ListBuffer(neigh))
          pathToNeigh.appendAll(pathTo)
          stack.addOne(neigh)
        })
      }
    }
    None
  }
}

object main2 {
  def main(args: Array[String]) = {
    val g = new G[Int](List(1,2,3,4, 5), Map(1 -> List(2, 3), 2 -> List(), 3 -> List(4), 4 -> List(), 5 -> List(1)))
    println(g.path(1, 5))
  }
}
