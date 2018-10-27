package GraphTransitionForCNF

/*
  Graph transition for CNF(Chomsky Normal Form) and graph defined in vertites marked as Int value from 0 to n

  Grammar input should be like:
          S A D F
          a s f
          S -> S A
          D -> S S
          A -> s
          F -> f
          i.e on first line all nonTerminals
              on second line all terminals
              on next lines rules for grammar  in CNF
  Graph input should be like:
          0 1 2 3 4 5
          0 1 f
          0 2 c
          1 2 f
          3 1 g
          1 3 d
          i.e on first line all vertices
              on next lines edges defines as <from> <to> <transition symbol>
* */
object Solution {
  type NonTerminal = String
  type Terminal = NonTerminal
  type Alphabet = String
  type ForwardProductions = Map[NonTerminal, Seq[List[Alphabet]]]

  class Edge(val from: Int, val to: Int, var transitionSymbol: Alphabet) {
    def canEqual(a: Any) = a.isInstanceOf[Edge]

    override def equals(that: Any): Boolean =
      that match {
        case that: Edge => that.canEqual(this) && this.hashCode == that.hashCode
        case _ => false
      }

    override def hashCode: Int = {
      val prime = 17
      var result = 1
      result = prime * result + from.hashCode()
      result = prime * result + to.hashCode()
      result = prime * result + transitionSymbol.hashCode
      result
    }
  }

  class OrientedGraph(val vertices: Set[Int], val edges: Set[Edge])

  class Grammar(val terminals: Set[NonTerminal], val NonTerminals: Set[Terminal],
                val productions: ForwardProductions) {

    var getMatrix: OrientedGraph => Array[Array[Set[Alphabet]]] = computeMatrixAsNonTerminals _ compose getInitialMatrix

    //reverse grammar
    val reverseProduction: Map[List[Alphabet], Set[NonTerminal]] = productions.toList
      .flatMap(pair => pair._2.map(value => Tuple2(value, pair._1)))
      .groupBy(_._1).map(elem => elem._1 -> elem._2.map(_._2).toSet)

    def isReachable(matrix: Array[Array[Set[Alphabet]]], from: Int, to: Int): Boolean = matrix(from)(to).nonEmpty

    def getInitialMatrix(graph: OrientedGraph): Array[Array[Set[Alphabet]]] = {
      val size = graph.vertices.size
      val matrix: Array[Array[Set[Alphabet]]] = (0 until size)
        .map(_ => (0 until size)
          .map(_ => Set.empty[Alphabet]).toArray).toArray
      for (edge <- graph.edges) {
        val key = List(edge.transitionSymbol.toString)
        if (reverseProduction.contains(key)) {
          matrix(edge.from)(edge.to) = reverseProduction(key)
        }
      }
      matrix
    }

    private def computeMatrixAsNonTerminals(matrix: Array[Array[Set[Alphabet]]]): Array[Array[Set[Alphabet]]] = {
      var isMatrixChanged = false
      var newMatrix = Array.empty[Array[Set[Alphabet]]]
      //newMatrix = M^2
      for (i <- matrix.indices) {
        var row = Array.empty[Set[Alphabet]]
        for (j <- matrix.indices) {
          var elem_i_j = Set.empty[Alphabet]
          for (k <- matrix.indices) {
            elem_i_j = elem_i_j.union(nonTerminalMul(matrix(i)(k), matrix(k)(j)))
          }

          if (nonTerminalSum(elem_i_j, matrix(i)(j)) != matrix(i)(j)) {
            isMatrixChanged = true
          }
          row :+= elem_i_j
        }
        newMatrix :+= row
      }
      // newMatrix =  M^2+M
      for (i <- matrix.indices) {
        for (j <- matrix.indices) {
          newMatrix(i)(j) = nonTerminalSum(newMatrix(i)(j), matrix(i)(j))
        }
      }

      if (isMatrixChanged) {
        computeMatrixAsNonTerminals(newMatrix)
      } else {
        newMatrix
      }
    }

    def nonTerminalMul(left: Set[Alphabet], right: Set[Alphabet]): Set[Alphabet] = {
      var acc = Set.empty[Alphabet]
      for (l <- left) {
        for (r <- right) {
          val key = List(l, r)
          if (reverseProduction.contains(key)) {
            acc = acc ++ reverseProduction(key)
          }
        }
      }
      acc
    }

    def nonTerminalSum(left: Set[Alphabet], right: Set[Alphabet]): Set[Alphabet] = left ++ right
  }


  // input like: n where on first line name of vertices
  // on n-1 -edges       from to transition symbols
  def parseInputToOrientedGraph(strings: Seq[String]): OrientedGraph = new OrientedGraph(
    strings.head.split(" +").map(_.toInt).toSet,
    strings.tail.map(_.split(" +").toSeq)
      .map(seq => new Edge(seq.head.toInt, seq.tail.head.toInt, seq.tail.tail.head)).toSet
  )


  // NFX(CNF)  without epsilon
  def parseInputToGrammar(strings: Seq[String]): Grammar = new Grammar(
    strings.head.split(" +").toSet,
    strings.tail.head.split(" +").toSet,
    strings.tail.tail.map(x => x.split(" +").toSeq).groupBy(_.head).mapValues(_.map(_.tail.toList))
  )


}
