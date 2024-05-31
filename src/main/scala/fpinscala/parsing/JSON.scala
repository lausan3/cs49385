package fpinscala.parsing

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def array = surround("[","]")(
      value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj = surround("{","}")(
      keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"
    def keyval = escapedQuoted ** (":" *> value)
    def lit = scope("literal") {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}

/*
 *   S -> (E)
 *   E -> T | E+T
 *   T -> F | T*F
 *   F -> I | (E)
 *   I -> a | b 
 */

trait AE
object AEInstance {

  case object AEPlus extends AE
  case object AETimes extends AE
  case class AEBinaryExpression(operator: AE, operand2: AE) extends AE
  case class AEExpressionList(expressions: List[AE]) extends AE
  case class AEFactor(expression: AE) extends AE
  case class AEIdentifier(value: Char) extends AE

  def additiveExpressionParser[Parser[+_]](P: Parsers[Parser]): Parser[AE] = {
    import P.{string => _,_}
    implicit def tok(s: String) = token(P.string(s))

    def S: Parser[AE] = surround("(",")")(E.map(v => AEExpressionList(List(v))))
    def E: Parser[AE] = 
      (T ** many( product(PL, T).map(v => AEBinaryExpression(AEPlus, v._2)) ))
        .map(v => AEExpressionList( v._1 :: v._2 )) scope "expression"
    def T: Parser[AE] = 
      (F ** many( product(TM, F)
        .map(v => AEBinaryExpression(AETimes, v._2)) ))
        .map(v => AEExpressionList( v._1 :: v._2 )) scope "term"
    def F = I | surround("(",")")(E.map(v => AEFactor(v))) scope "factor"
    def I = or("a", "b").map(v => AEIdentifier(v.charAt(0))) scope "identifier"
    def PL = "+".as(AEPlus)
    def TM = "*".as(AETimes)

    root(whitespace *> S <* whitespace)
  }
}

/*
 *   regular grammar
 *
 *   S -> aS | bA
 *   A -> cA | c
 *
 */

trait RG1
object RG1_Instance {

  case class RG1_Sequence(tokens: List[RG1]) extends RG1
  case object RG1_A extends RG1
  case object RG1_B extends RG1
  case object RG1_C extends RG1

  def RG1Parser[Parser[+_]](P: Parsers[Parser]): Parser[RG1] = {
    import P.{string => _,_}
    implicit def tok(s: String) = token(P.string(s))

    def S: Parser[RG1_Sequence] = 
      surround("(",")")(SS.map(v => RG1_Sequence(List(v))))
    def SS: Parser[RG1_Sequence] = 
      or( 
        product(a, many(SS).map(v => RG1_Sequence(v)))
          .map(v => RG1_Sequence(v._1 :: v._2.tokens )), 
        product(b, A)
          .map(v => RG1_Sequence(v._1 :: v._2.tokens)) 
      )
    def A: Parser[RG1_Sequence] = 
      product(c, many(A).map(v => RG1_Sequence(v))).map(v => RG1_Sequence(v._1 :: v._2.tokens))
    def a = "a".as(RG1_A)
    def b = "b".as(RG1_B)
    def c = "c".as(RG1_C)
    root(whitespace *> S <* whitespace)
  }
}

/**
 * JSON parsing example.
 */
object JSONExample /*extends App*/ {

  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  val P = fpinscala.parsing.Reference
  import fpinscala.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E,JSON]) =
    e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(P)
  printResult { P.run(json)(jsonTxt) }
  println("--")
  printResult { P.run(json)(malformedJson1) }
  println("--")
  printResult { P.run(json)(malformedJson2) }
}


