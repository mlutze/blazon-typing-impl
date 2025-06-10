import Typer.Error
import grammar.terminal.{Accent, Attitude, MobileCharge}
import org.bitbucket.inkytonik.kiama.util.Positions

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.io.StdIn

object Main {

  private val Prompt: String = "> "

  def main(args: Array[String]): Unit = {
    args match {
      case Array("test", file) =>
        printHuman(runTest(file))

      case Array("stats") =>
        printStats()

      case _ =>
        runRepl()
    }
  }


  private case class TestResult(
                                 successes: Int,
                                 parseErrors: Int,
                                 typeErrors: Map[Class[_ <: Typer.Error], Int]
                               )

  private def runRepl(): Unit = {
    Iterator.continually {
        print("> ")
        StdIn.readLine()
      }
      .takeWhile(_ != null)
      .foreach {
        line =>
          printingErrs(line)(checkString)
      }
  }

  private def printingErrs(input: String)(f: String => Unit): Unit = {
    try {
      f(input)
      println("OK")
    } catch {
      case e: Parser.Error =>
        reportParseError(input, e)
      case e: Typer.Error =>
        reportTypeError(input, e)
      case e: Exception =>
        reportOtherError(e)
    }
  }

  private def reportParseError(input: String, e: Parser.Error): Unit = {
    println(" " * (e.pos.column - 1 + Prompt.length) + "^")
    println("Parse error:")
  }

  private def reportTypeError(input: String, e: Typer.Error): Unit = {
    val (pos1, pos2) = e.loc
    val len = pos2.column - pos1.column
    println(" " * (pos1.column - 1 + Prompt.length) + "^" * len)
    println("Type error: " + getTypeErrorString(e))
  }

  private def reportOtherError(e: Exception): Unit = {
    println("Unexpected exception: " + e)
  }


  private def getTypeErrorString(e: Typer.Error): String = e match {
    case Error.RepeatedTincture(loc) => "repeated tincture"
    case Error.UndefinedTinctureReference(loc) => "undefined tincture reference"
    case Error.RepeatedCount(loc) => "repeated count"
    case Error.UndefinedCountReference(loc) => "undefined count reference"
    case Error.RuleOfTinctureSingle(loc) => "rule of tincture violation"
    case Error.RuleOfTinctureMixed(loc) => "rule of tincture violation"
    case Error.TrivialVariation(loc) => "trivial variation"
    case Error.OddVariation(loc) => "odd variation"
    case Error.ImpossibleAlignment(loc) => "impossible alignment"
    case Error.InvalidAttitudeOrAccent(loc) => "invalid attitude or accent"
  }

  private def runTest(file: String): TestResult = {
    var successes = 0
    var parseErrs = 0
    val typeErrs = mutable.Map.empty[Class[_ <: Typer.Error], Int].withDefaultValue(0)
    Files.lines(Path.of(file)).forEach {
      line =>
        try {
          checkString(line)
          successes += 1
        } catch {
          case e: Parser.Error => parseErrs += 1
          case e: Typer.Error => typeErrs(e.getClass) += 1
          case _ =>
            // TODO we assume other errors are due to other problems with parsing
            parseErrs += 1
        }
    }
    TestResult(successes, parseErrs, typeErrs.toMap)
  }

  private def checkString(string: String): Unit = {
    implicit val pos: Positions = new Positions()
    val ast = Parser.parse(string)
    Typer.check(ast)
  }

  private def printHuman(res: TestResult): Unit = {
    val total = res.successes + res.parseErrors + res.typeErrors.values.sum
    println(s"Total: $total")
    println(s"Parsed: ${total - res.parseErrors}")
    println(s"Typed: ${res.successes}")
    println(s"Parse Errors: ${res.parseErrors}")
    res.typeErrors.foreach {
      case (clazz, count) =>
        val name = clazz.getSimpleName
        println(s"Type Errors ($name): $count")
    }
  }

  private def printStats(): Unit = {
    println("Accents: " + Accent.values.length)
    println("Attitudes: " + Attitude.values.length)
    println("Mobile Charges: " + MobileCharge.values.length)
  }
}
