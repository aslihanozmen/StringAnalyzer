package services

import ch.epfl.lara.synthesis.stringsolver.Program.Program
import ch.epfl.lara.synthesis.stringsolver.StringSolver.Input_state
import ch.epfl.lara.synthesis.stringsolver.{Printer, StringSolver, StringSolverAlgorithms}

import collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object SyntaxicMatch {

  final case class TransformMultipleInputs( valid: Boolean, how: (java.util.List[String]) => String, score: Int)
  final case class MatchResponse( valid: Boolean, how: (String) => String)
  final case class MatchInputsResponse( valid: Boolean, how: (java.util.List[String]) => String, score: Int)

  def unitTransformInput() =TransformMultipleInputs(true, (content: java.util.List[String]) => content.asScala(0), 0)


  //matching score
  def score(inputs: java.util.List[String], output: String): Int ={
    var score =0;

    for( input <- inputs.asScala.map(_.toLowerCase)){

      val AComb=input.inits.flatMap(_.tails.toList.init).filter(_.size > 1).toList
      for( test <- AComb){
        if( output.toLowerCase.contains(test) ){
          score = score + test.size
        }
      }
    }
    score
  }


  val cacheTransformMultipleInputs= collection.mutable.Map[(List[String],String), TransformMultipleInputs]()
  def transformMultipleInputs( input: java.util.List[String], output: String): TransformMultipleInputs = {
    cacheTransformMultipleInputs.getOrElseUpdate((input.asScala.toList,output), _transformMultipleInputs(input,output))
  }

  // combine multiple input into a single output
  def _transformMultipleInputs( input: java.util.List[String], output: String): TransformMultipleInputs = {
    val stringSolver=new StringSolver()
    stringSolver.setLoopLevel(0);
    stringSolver.add(input.asScala,output)


    TransformMultipleInputs( true, (content) => {
      val r=stringSolver.solve(
        content.asScala)
      r(0)
    },score(input, output))
  }

  val cacheInputsMatch= collection.mutable.Map[(List[String],String), MatchInputsResponse]()
  def inputsMatch( input: java.util.List[String], output: String): MatchInputsResponse = {
    cacheInputsMatch.getOrElseUpdate((input.asScala.toList,output), _inputsMatch(input,output))
  }


  def _inputsMatch( inputs: java.util.List[String], output: String): MatchInputsResponse = {
    val stringSolver=new StringSolver()
    val scalainputs = inputs.asScala
    stringSolver.setLoopLevel(0)

    if( score(inputs, output) == 0){
      MatchInputsResponse(false, (content) => content.asScala(0),-1)
    }else {
      stringSolver.add(scalainputs, output)

      if (stringSolver.solve(scalainputs.map(_.map(_ => 'x')))(0).equalsIgnoreCase(output)) {
        MatchInputsResponse(false, (content) => stringSolver.solve(content.asScala)(0), -1)
      } else {
        MatchInputsResponse(true, (content) => stringSolver.solve(content.asScala)(0), score(inputs, output))
      }
    }
  }

  def possibleMatch( input: String, output: String): MatchResponse = {
    val f=new StringSolverAlgorithms()
    val result = f.generateStr(Input_state(IndexedSeq(input),0), output, 0)

    // check if give us a good result with random String => not a valid transformer.
    Try{
      result.takeBest("xxxxxxx")
    } match {
      case Failure(exception) =>
        MatchResponse(true, (input:String) => result.takeBest(input))
      case Success(response) =>
        if( response.equalsIgnoreCase(output)){
          MatchResponse(false, (_ => ""))
        }else
          MatchResponse(true, (input:String) => result.takeBest(input))
    }
  }
}
