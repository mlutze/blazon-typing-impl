import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.util.Positions
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class TypeCorpus extends AnyFunSuite with BeforeAndAfterAll {

  private val PassedPath = Path.of("target/passed.txt")
  private val FailedPath = Path.of("target/failed.txt")

  // temporary filter for isolating tests
  private def shouldTest(blazon: String): Boolean = {
    true
    //    blazon.contains("and a ")
  }

  override def beforeAll(): Unit = {
    Files.deleteIfExists(FailedPath)
    Files.createFile(FailedPath)
    Files.deleteIfExists(PassedPath)
    Files.createFile(PassedPath)
  }

  Files.lines(Path.of("src/test/scala/corpus.txt")).distinct().forEach {
    blazon =>
      // we ignore incomplete and annotated blazons
      val badIndicators = Set("(", ")", "?", "...")
      val cleanBlazon = blazon.strip()
      if (badIndicators.exists(blazon.contains(_)) || !shouldTest(blazon)) {
        ignore(cleanBlazon) {}
      } else {
        implicit val pos = new Positions()
        Parser(pos).parseString(cleanBlazon) match {
          case Success(ast, _) =>
            test(cleanBlazon) {
              try {
                Typer.check(ast)
              } catch {
                case e: Typer.Error =>
                  failTypeError(cleanBlazon, e)
              }
            }
          case _: NoSuccess =>
            ignore(cleanBlazon) {}
        }
      }
  }

  private def failTypeError(input: String, e: Typer.Error): Unit = {
    val (pos1, pos2) = e.loc
    val len = pos2.column - pos1.column
    val lines = List(
      input,
      " " * (pos1.column - 1) + "^" * len,
      "Type error: " + getTypeErrorString(e),
    )
    fail(lines.mkString("\n") + "\n")
  }

  private def getTypeErrorString(e: Typer.Error): String = e match {
    case Typer.Error.RepeatedTincture(loc) => "repeated tincture"
    case Typer.Error.UndefinedTinctureReference(loc) => "undefined tincture reference"
    case Typer.Error.RepeatedCount(loc) => "repeated count"
    case Typer.Error.UndefinedCountReference(loc) => "undefined count reference"
    case Typer.Error.RuleOfTinctureSingle(loc) => "rule of tincture violation"
    case Typer.Error.RuleOfTinctureMixed(loc) => "rule of tincture violation"
    case Typer.Error.TrivialVariation(loc) => "trivial variation"
    case Typer.Error.OddVariation(loc) => "odd variation"
    case Typer.Error.ImpossibleAlignment(loc) => "impossible alignment"
    case Typer.Error.InvalidAttitudeOrAccent(loc) => "invalid attitude or accent"
  }
}
