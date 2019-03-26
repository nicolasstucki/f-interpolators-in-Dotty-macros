import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

abstract class MacroStringInterpolator[T] {
  final def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] = {
    try interpolate(strCtxExpr, argsExpr)
    catch {
      case ex: NotStaticlyKnownError =>
        throw new QuoteError(ex.getMessage)
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
    * Computes a StringContext given an Expression of it
    * @param strCtxExpr the given expression containing the StringContext
    * @return the computed StringContext
    * @throws NotStaticlyKnownError if the elements of the StringContext are trees or if the unsealed expression is a tree
    */
  protected def getStaticStringContext(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): StringContext = {
    import reflect._
    strCtxExpr.unseal.underlyingArgument match {
      case Term.Select(Term.Typed(Term.Apply(_, List(Term.Apply(_, List(Term.Typed(Term.Repeated(strCtxArgTrees, _), TypeTree.Inferred()))))), _), _) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Term.Literal(Constant.String(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
        }
        StringContext(strCtxArgs: _*)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
    }
  }

  /**
   * Computes a list of expr from a given expr of sequence of elements
   * @param argsExpr the given expression
   * @return the computed list
   * @throws NotStaticlyKnownError if the elements of the list are trees
   */
  protected def getArgsList(argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): List[Expr[Any]] = {
    import reflect._ 
    // TODO : use this for the position of the argument 
    argsExpr.unseal.underlyingArgument match {
      case Term.Typed(Term.Repeated(args, _), _) => args.map(_.seal[Any])
      case tree => throw new NotStaticlyKnownError("Expected statically known argument list", tree.seal[Any])
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