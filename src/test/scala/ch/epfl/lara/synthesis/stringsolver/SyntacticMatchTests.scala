package ch.epfl.lara.synthesis.stringsolver

import org.scalatest._
import org.scalatest.matchers._
import services.SyntaxicMatch
import collection.JavaConverters._
class SyntacticMatchTests extends FlatSpec with ShouldMatchers with PrivateMethodTester {
  import Evaluator._
  import Implicits._
  import Program._
  import ProgramSet._
  import StringSolver._

  val generateStr = PrivateMethod[STraceExpr]('generateStr)

  val f = new StringSolverAlgorithms()
import f._
  initStats()

  it should "Test Solver" in {


    val Z=SyntaxicMatch.transformMultipleInputs(List("0.8202", "10 USD").asJava,"0.82 * 10");
    println(Z.how( List("0.810", "20 USD").asJava))
    println(Z.how( List("0.22", "20 USD").asJava))
    println(Z.how( List("0.44", "20 USD").asJava))

    val stringSolver=new StringSolver()
    stringSolver.add(List("0.8202", "10 USD"), "0.82 * 10")
    val response = stringSolver.solve(List("0.1202", "20 USD"))
    println("response:"+response);

    val tt=new StringSolver()
    tt.add(List("microsoft","xerox"), "microsoft xerox")
    println("solve:"+tt.solve(List("gg","hh")))

    val f=new StringSolverAlgorithms()



    //test output generation
    val output=new StringSolverAlgorithms()
    output.generateStr( Input_state(IndexedSeq("microsoft"),0), "microsfot xerox",0)
    val routput=output.generateStr( Input_state(IndexedSeq("microsoft"),1), "microsfot xerox",0)

    println("---"+routput.takeBest( IndexedSeq("microsoft"),0))
    println("---"+routput.takeBest( IndexedSeq("xs", "gg"),0))



    val result1 = f.generateStr( Input_state(IndexedSeq("c4 c3 c1"),0), "c1",0)
    val result2 = f.generateStr( Input_state(IndexedSeq("c4 c3 c1"),0), "microsoft",0)

    result2.takeBest("xxxxxxx").equalsIgnoreCase("")
/*
    val c = StringSolver()
    c.add("microsoft hola", "microsfot hola")
    c.solve("ok bm")(0) should equal ("ok bm")*/


    val result3 = f.generateStr( Input_state(IndexedSeq("microsoft hola"),0), "hola microsoft",0)
    println("---:"+result3.takeBest("samsing ok"))

    println("result1:"+result1)
    println("result2:"+result2)
    println(result2.takeBest("c1"))
    println(result2.takeBest("c6"))

    val res3 = intersect(result1, result2)
    println(result2.takeBest("c6"))
  }
}

