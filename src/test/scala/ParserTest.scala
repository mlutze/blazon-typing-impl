import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("Test.Pos.Tincture") {
    parseSuccess("Gu.")
  }

  test("Test.Pos.Division") {
    parseSuccess("Per pale arg. and sa.")
  }

  test("Test.Neg.Lowercase") {
    parseFailure("gu.")
  }

  test("TempTest") {
    parseSuccess("Gu. three crosses botonny in bend or.")
  }

  private def parseSuccess(s: String): Unit = {
    Parser().parseString(s) match {
      case _: Success[_] => ()
      case failure: NoSuccess => fail(failure.message + " at " + failure.next.nextPosition.format)
    }
  }

  def parseFailure(s: String): Unit = {
    Parser().parseString(s) match {
      case _: Success[_] => fail("erroneously succeeded parsing")
      case _: NoSuccess => ()
    }
  }
}