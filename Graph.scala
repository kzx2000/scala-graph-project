package graph

import collection.mutable
import scala.collection.mutable.ArrayBuffer

object Graph{
  case class Edge(val dest : Vertex, val weight : Int)

  class Vertex{
    val edges = mutable.Buffer[Edge]()
  }

  val graph = Array.fill(4)(new Vertex)
  graph(0).edges += Edge(graph(1),2) += Edge(graph(2),3)

  val adjMatrix = Array(
    Array(0,1,0,0),
    Array(0,0,3,5),
    Array(2,0,0,1),
    Array(1,0,0,0))

  // Function to determine whether there is a path between two vertex
  // Takes the current location, the destination and the graph.
  // It should return a boolean whether the destination vertex is reachable or not

  def canReach(currentVertex : Int, destinationVertex : Int, graph : Array[Array[Int]], visited : Array[Boolean]): Boolean ={
    // add the current vertex into the visited list
    visited(currentVertex) = true
    if(currentVertex == destinationVertex) true
    else{
      var r = false
      for(i <- graph(currentVertex).indices){
        if(graph(currentVertex)(i) > 0 && !(visited(i))){
          r ||= canReach(i, destinationVertex, graph, visited)
        }
      }
      r
      // first issue w. this code if it exists a direct cycle inside the algorithm loops to infinity
      // we would like to add a 'visited' flag not to loop and so a list of our visited vertex
      // another possible solution : replace the for loop. It also stop at the first true state rather the for loop goes
      // until the end

      //graph(currentVertex).indices.exists(i => graph(currentVertex)(i) > 0 && canReach(i, destinationVertex, graph))
    }
  }

  def adj_Djikstra(srcVertex : Int, destinationVertex : Int, G : Array[Array[Int]]): List[Int] ={

    // add can reachFunction
    if(canReach(srcVertex, destinationVertex, G, Array.fill(G.size)(false))) {
      // initialize current node
      var currentNode = srcVertex

      // declare a list of vertex to evaluate
      var nodes = List.range(0, G.length)
      var parentNode = List.fill(G.length)(0)
      // declare a weight for each node
      var w = List.fill(G.length)(Int.MaxValue)

      // update w vector : srcVertex -> srcVertex = 0
      w = w.updated(currentNode, 0)

      while (!(nodes.isEmpty)) {
        // determine neighbors' indices
        val neighbors = for (i <- G(currentNode).indices if G(currentNode)(i) > 0) yield i

        for (neighbor <- neighbors) {
          // for each neighbor update the value of the path if actual weight greater than the weight of parent node + weighted link
          if (w(neighbor) > w(currentNode) + G(currentNode)(neighbor)) {
            // weight update
            w = w.updated(neighbor, G(currentNode)(neighbor) + w(currentNode))
            // parent node update
            parentNode = parentNode.updated(neighbor, currentNode)
          }
        }

        // delete current node for nodes
        nodes = nodes.filter(_ != currentNode)

        // determine new current node by taking the smallest values contains in w and in node
        if (!(nodes.isEmpty)) {
          val map = for (i <- nodes) yield (i, w(i))
          currentNode = map.minBy(_._2)._1
        }


      }
      return w
    } else {
      /// return an error
      /// to do
      return List.fill(G.size)(0)
    }
  }

  def main(args: Array[String]): Unit = {
    //println(canReach(0,3,adjMatrix, Array.fill(adjMatrix.size)(false)))
    println(adj_Djikstra(0,3, adjMatrix))
  }
}
