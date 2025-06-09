import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}

class ParseCorpus extends AnyFunSuite with BeforeAndAfterAll {

  private val PassedPath = Path.of("target/passed.txt")
  private val FailedPath = Path.of("target/failed.txt")

  // temporary filter for isolating tests
  private def shouldTest(blazon: String): Boolean = {
    true
//    blazon.contains(" one ")
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
        ignore(cleanBlazon){}
      } else {
        test(cleanBlazon) {
          parseSuccess(cleanBlazon)
        }
      }
  }

  private def parseSuccess(s: String): Unit = {
    Parser().parseString(s) match {
      case _: Success[_] =>
        Files.writeString(PassedPath, s + "\n", StandardOpenOption.APPEND)
      case failure: NoSuccess =>
        Files.writeString(FailedPath, s + "\n", StandardOpenOption.APPEND)
        val col = failure.next.nextPosition.column
        val message = List(
          "",
          failure.message,
          "",
          s,
          (" " * (col - 2)) + "^",
          "",
        ).mkString("\n")
        fail(message)
    }
  }
}
