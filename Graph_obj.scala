package graph

// Représentation d'un graphe par noeuds

object Graph_obj {

  // list of nodes contained in the graph
  var nodes = collection.mutable.Map[String,Node]()

  // Define node object
  class Node (id : String){

    // Direct neighbors of this node
    private var neighbors = collection.mutable.Map[Node, Int]()

    // Dijkstra variable attached to node object is used to memorize the previous node in the shortest path and the total
    // path cost to arrive to this node
    private var dijkstra : (Node, Int) = (null, 0)

    // var visited to memorize either the node has been already visited during the algorithme
    private var visited = false

    // Get the total path cost
    def getCost() : Int = this.dijkstra._2

    // Get and set node state (Visited : true || false)
    def getState() : Boolean =  this.visited
    def setState(b : Boolean) : Unit = this.visited = b

    // Get and set Djikstra values (Previous node and total cost)
    def setDijkstra(p : Node, w : Int) : Unit = this.dijkstra = (p, w)
    def getDijkstra() : (Node, Int) = this.dijkstra

    // Get node id
    def getId() : String = this.id

    // Get node object
    def getNode(id : String) : Node = this

    // Add neighbor node to list
    def addNeighbors(V : Node, W : Int) : Unit = {
      this.neighbors += (V -> W)
    }

    // Get neighbor list
    def getNeighbors() : collection.mutable.Map[Node, Int] = neighbors

    // toString -> return node information type String
    override def toString: String = {
      var res = "Node " + this.id + " has " + this.neighbors.size + " neighbor"

      for ((i, j) <- this.neighbors ; if(!this.neighbors.isEmpty)) {
        res += "\n - " + i.getId() + " distance of " + j
      }
      return res + "\nState : " + this.visited +"\n Dijkstra Cost : " + this.dijkstra._2 + "\n Dijkstra previous neighbor : " + this.dijkstra._1.getId()
    }
  }

  // addNodes
  // @param : Node List
  // @return : Unit
  // add nodes to graph node list

  def addNodes(nx : List[Node]) : Unit = nx match {
    case head::tail => {
      nodes += (head.getId()-> head)
      addNodes(tail)
    }
    case Nil => Unit
  }

  // define some nodes
  val nodeA = new Node("A")
  val nodeB = new Node("B")
  val nodeC = new Node("C")
  val nodeD = new Node("D")
  val nodeE = new Node("E")
  val nodeF = new Node("F")

  // add nodes to the graph
  addNodes(List(nodeA, nodeB, nodeC, nodeD, nodeE, nodeF))

  // add node neighbors
  nodeA.addNeighbors(nodeB, 1)
  nodeA.addNeighbors(nodeC, 2)
  nodeB.addNeighbors(nodeD, 4)
  nodeD.addNeighbors(nodeE, 6)
  nodeC.addNeighbors(nodeE, 5)
  nodeC.addNeighbors(nodeF, 1)
  nodeF.addNeighbors(nodeD, 1)
  nodeF.addNeighbors(nodeE, 2)

  // toString
  // define the current graph
  override def toString: String = {
    return("This graph contains " + nodes.size + " nodes")
  }

  // findNode
  // @params : String
  // @return : Node
  // find a node object contained in the graph by its String id
  def findNode(s : String): Node = {
    var returnNode : Node = null
    for (n <- nodes; if n._1 == s){
      returnNode = n._2
    }
    if(returnNode != null) return returnNode
    else throw new IllegalArgumentException
  }

  // findMin
  // @params : map((Node, Node), Int)
  // @return : map((Node, Node), Int)
  // @return : ((Node, Node), Int)
  // findMin return the minimum element contained in the stack ((Node, Node), Int) -> ((Node, its previous Node),
  // total cost to come to the node
  def findMin(m : collection.mutable.Map[(Node, Node),Int]) : ((Node, Node), Int) = {
    var min : ((Node, Node),Int) = m.head
    for(n <- m; if min._2 > n._2) min = n
    return(min)
  }

  // Print the path from a source node to a destination
  def djikstraPath(src : Node, dest : Node) : collection.mutable.ListBuffer[Node] = {
    var path = new collection.mutable.ListBuffer[Node]()
    path += dest
    var current_node : Node = dest
    while(current_node != src){
      path += current_node.getDijkstra()._1
      current_node = current_node.getDijkstra()._1
    }
    path = path.reverse

    return path
  }

  // Implementation of the Djikstra Algorithm
  // @params : Node -> Source node
  // @params : Node -> Destination node
  // @return : Unit -> for now, could be good to return the list node contains in the Dijkstra path

  def dijkstra(src : String, dest : String) : List[Node] = {
    val node_src = findNode(src)
    var current_node = node_src
    val node_dest = findNode(dest)

    // Map node stack to memorize the accessible node
    // format : ((Node, Node),Int) -> (Accessible Node, the previous node to come to it), the total cost to come to it)
    var node_stack = collection.mutable.Map[(Node, Node),Int]()

    // While the current is not marked visited or the current node is not the final destination node then keep looping
    while (!current_node.getState() && current_node.getId() != node_dest.getId()) {
      // set current node to visited
      current_node.setState(true)

      // get node neighbors
      val neighbors = current_node.getNeighbors()

      // if neighbors is not empty
      if(!neighbors.isEmpty) {
        // for each neighbor not visited in neighbors list, add them into the node stack with : (Node, the previous node),
        // path cost from previous to node + current node cost
        for (node <- neighbors; if !node._1.getState()) node_stack += ((node._1, current_node) -> (node._2 + current_node.getCost()))

        // find the minimum node from the node stack regarding the memorized cost
        val min : ((Node, Node), Int)= findMin(node_stack)

        // Delete minimun element from the stack
        node_stack -= min._1
        // update minimum node Djikstra variable with its previous node and the total cost to come to it
        min._1._1.setDijkstra(min._1._2,min._2)
        // set the minimum node as the current node of the algorithm
        current_node = min._1._1
      }
    }
    println(node_dest)
    //printPath(node_src, node_dest)
    val path : List[Node] = djikstraPath(node_src, node_dest).toList
    return path
  }

  def pathToString(nx: List[Graph_obj.Node]) : String = nx match {
    case head::Nil => head.getId() + " -- Path cost : " + head.getCost()
    case head::tail => head.getId() + "->" + pathToString(tail)
    case Nil => ""
  }

  def main(args: Array[String]): Unit = {
    // fonction define graph
    try {
      val path : List[Node] = dijkstra("A", "D")
      println(pathToString(path))
    } catch {
      case e: IllegalArgumentException => println("Impossible de trouver le noeud dans le graph ! ")
    }

  }

}
