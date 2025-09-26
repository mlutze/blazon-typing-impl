import grammar.Blazon
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.util.Positions
import org.scalatest.Assertions
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.ClassTag

class TyperTest extends AnyFunSuite with Assertions {

  test("Test.Pos.Tincture") {
    typeSuccess("Gu.")
  }

  test("Test.Pos.Division") {
    typeSuccess("Per pale arg. and sa.")
  }

  test("Test.Neg.NoRef") {
    typeFailure(classOf[Typer.Error.UndefinedTinctureReference])("Gu. a lion of the second.")
  }

  test("Test.Neg.Tincture") {
    typeFailure(classOf[Typer.Error.RuleOfTinctureSingle])("Gu. a lion az.")
  }

  test("Test.Regression.01") {
    typeSuccess("Gu. a cross potent arg.")
  }

  test("Test.Regression.02") {
    typeSuccess("Gu. on a bend between two lions arg. a mullet of the first.")
  }

  test("Test.Regression.03") {
    typeFailure(classOf[Typer.Error.RuleOfTinctureSingle])("Gules a chevron azure.")
  }

  private def typeSuccess(s: String): Unit = {
    implicit val pos = new Positions()
    Parser(pos).parseString(s) match {
      case Success(b, _) =>
        Typer.check(b)
      case failure: NoSuccess => fail(failure.message + " at " + failure.next.nextPosition.format)
    }
  }

  private def typeFailure(e: Class[_])(s: String) = {
    implicit val pos = new Positions()
    Parser(pos).parseString(s) match {
      case Success(b, _) =>
        val err = intercept[Typer.Error](Typer.check(b))
        assert(e === err.getClass)
      case failure: NoSuccess => fail(failure.message + " at " + failure.next.nextPosition.format)
    }
  }

  private def parseFailure(s: String): Unit = {
    implicit val pos = new Positions()
    Parser(pos).parseString(s) match {
      case _: Success[_] => fail("erroneously succeeded parsing")
      case _: NoSuccess => ()
    }
  }
}