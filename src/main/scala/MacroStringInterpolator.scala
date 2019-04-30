import scala.quoted._
import scala.quoted.matching._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import reflect._

abstract class MacroStringInterpolator[T] {
  final def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] = {
    try interpolate(strCtxExpr, argsExpr)
    catch {
      case ex: NotStaticlyKnownError =>
        QuoteError(ex.getMessage)
    }
  }

  /**
    * Interpolates the given arguments to the formatted string
    * @param strCtxExpr the expression that holds the StringContext which contains all the chunks of the formatted string
    * @param args the expression that holds the sequence of arguments to interpolate to the string in the correct format
    * @return the expression containing the formatted and interpolated string
    * @throws TastyTypecheckError if the given format is not correct
    */
  protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T]

  /**
   * Computes a list of expr from a given expr of sequence of elements
   * @param argsExpr the given expression
   * @return the computed list
   * @throws NotStaticlyKnownError if the elements of the list are trees
   */
  protected def getArgsList(argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): List[Expr[Any]] = {
    argsExpr match {
      case Repeated(args) =>
        args.toList
      case _ => 
        List('{ "ERROR" })
    }
  }

  protected implicit def StringContextIsLiftable: Liftable[StringContext] = new Liftable[StringContext] {
    def toExpr(strCtx: StringContext): Expr[StringContext] = {
      // TODO define in stdlib?
      implicit def ListIsLiftable: Liftable[List[String]] = new Liftable[List[String]] {
        override def toExpr(list: List[String]): Expr[List[String]] = list match {
          case x :: xs => '{${x.toExpr} :: ${toExpr(xs)}}
          case Nil => '{Nil}
        }
      }
      '{StringContext(${strCtx.parts.toList.toExpr}: _*)}
    }
  }

  protected class NotStaticlyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)
}