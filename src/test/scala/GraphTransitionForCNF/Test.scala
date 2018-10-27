package GraphTransitionForCNF


import GraphTransitionForCNF.Solution.Edge
import org.scalatest.{FlatSpec, Matchers}

class Test extends FlatSpec with Matchers {
  val program = Solution
  val grammarToParse = Seq(
    "S B C D E",
    "b c e",
    "S C D",
    "B D E",
    "D C D",
    "B b",
    "D E",
    "C c",
    "E e",
  )
  // 0->1 by b
  // 0->2 by c
  // 2->1 by e
  //3->2 by b
  // 3-> 1 by c
  //2->0 by b
  //1 -> 3 by b
  val graphToParse = Seq(
    "0 1 2 3",
    "0 1 b",
    "0 2 c",
    "2 1 e",
    "3 2 b",
    "3 1 c",
    "2 0 b",
    "1 3 b",
  )

  "Parse of correct Grammar Input" should "Parse grammar" in {

    val parsedGrammar = program.parseInputToGrammar(grammarToParse)

    parsedGrammar.terminals shouldEqual Set("S", "B", "C", "D", "E")
    parsedGrammar.NonTerminals shouldEqual Set("c", "e", "b")
    parsedGrammar.productions shouldEqual Map(
      "S" -> Seq(List("C", "D")),
      "B" -> Seq(List("D", "E"), List("b")),
      "C" -> Seq(List("c")),
      "D" -> Seq(List("C", "D"), List("E")),
      "E" -> Seq(List("e")),
    )
    parsedGrammar.reverseProduction shouldEqual Map(
      List("D", "E") -> Set("B"),
      List("C", "D") -> Set("S", "D"),
      List("b") -> Set("B"),
      List("c") -> Set("C"),
      List("E") -> Set("D"),
      List("e") -> Set("E")
    )
  }
  "Parse of correct Graph Input" should "Parse Graph" in {

    val parsedGraph = program.parseInputToOrientedGraph(graphToParse)

    parsedGraph.vertices shouldBe Set(0, 1, 2, 3)
    parsedGraph.edges should contain theSameElementsAs Set(
      new Edge(0, 1, "b"),
      new Edge(0, 2, "c"),
      new Edge(2, 1, "e"),
      new Edge(3, 2, "b"),
      new Edge(3, 1, "c"),
      new Edge(2, 0, "b"),
      new Edge(1, 3, "b"),
    )
  }

  "Mul of sets with productions" should "Correct mul" in {
    val grammar = program.parseInputToGrammar(grammarToParse)
    grammar.nonTerminalMul(Set("C", "D"), Set("D", "E")) shouldBe Set("S", "D", "B")
  }
  "Sum of sets with productions" should "Correct mul" in {
    val grammar = program.parseInputToGrammar(grammarToParse)
    grammar.nonTerminalSum(Set("C", "D"), Set("D", "E")) shouldBe Set("C", "D", "E")
  }

  "Get initial matrix" should "return tranjesacn matrix" in {
    val grammar = program.parseInputToGrammar(grammarToParse)
    val matrix = grammar.getInitialMatrix(program.parseInputToOrientedGraph(graphToParse))
    matrix shouldBe Array(
      //E DE CD
      //  0 1 2 3
      Array(Set.empty[String], Set("B"), Set("C"), Set.empty[String]), // 0
      Array(Set.empty[String], Set.empty[String], Set.empty[String], Set("B")), // 1
      Array(Set("B"), Set("E"), Set.empty[String], Set.empty[String]), // 2
      Array(Set.empty[String], Set("C"), Set("B"), Set.empty[String]), // 3
    )


  }

  "Computation of matrix without step" should "Compute matrix" in {
    val graph = program.parseInputToOrientedGraph(graphToParse)
    val grammar = program.parseInputToGrammar(grammarToParse).getMatrix(graph) shouldBe Array(
      Array(Set.empty[String], Set("B"), Set("C"), Set.empty[String]), // 0
      Array(Set.empty[String], Set.empty[String], Set.empty[String], Set("B")), // 1
      Array(Set("B"), Set("E"), Set.empty[String], Set.empty[String]), // 2
      Array(Set.empty[String], Set("C"), Set("B"), Set.empty[String]), // 3
    )
  }

  "Computation of matrix without huge steps" should "Compute matrix" in {
    // create grammar for our
    // create graph
    val graphToParse = Seq(
      "0 1 2 3",
      "0 1 a",
      "1 2 a",
      "2 0 a",
      "1 3 b",
      "3 1 b"
    )
    val grammarToParse = Seq(
      "S A S' B",
      "a b",
      "S A S'",
      "S' S B",
      "S A B",
      "A a",
      "B b"
    )
    val grammar = program.parseInputToGrammar(grammarToParse)
    val expectedMatrix = grammar.getMatrix(program.parseInputToOrientedGraph(graphToParse))
    expectedMatrix shouldBe Array(
      Array(Set(), Set("S", "S'", "A"), Set(), Set("S'", "S")),
      Array(Set(), Set("S", "S'"), Set("A"), Set("S'", "S", "B")),
      Array(Set("A"), Set("S", "S'"), Set(), Set("S", "S'")),
      Array(Set(), Set("B"), Set(), Set())
    )

  }
}
