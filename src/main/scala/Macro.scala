
import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

object Macro {

  class StringContextOps(strCtx: => StringContext) {
    inline def f2(args: Any*): String = ~FIntepolator('(strCtx), '(args))
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)
}

object FIntepolator extends MacroStringInterpolator[String] {
  
  override protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    import scala.tasty.TastyTypecheckError

    /**
      * Checks if a given type is a subtype of any of the possibilities
      * @param tpe the given type 
      * @param possibilities all the types within which we want to find a super type of tpe
      * @return true if the given type is a subtype of at least one of the possibilities, false otherwise
      */
    def checkSubtype(tpe : Type, possibilities : Type*) : Boolean = {
      possibilities.find(tpe <:< _).nonEmpty
    }

    /**
      * Checks if a given character is a kind of flag for the formatting of a string
      * @param c the given character
      * @return true if the given character is a flag, false otherwise
      */
    def isFlag(c : Char) : Boolean = c match {
      case '-' | '#' | '+' | ' ' | '0' | ',' | '(' => true
      case _ => false
    }

    /**
     * Go through the whole given string until it find the formatting string 
     * and returns the corresponding index
     * @param s the given string containing the formatting string as substring
     * @param argPos the position of the argument to format, only useful to throw errors
     * @throws TastyTypecheckError if the formatting string has not the correct format
     * @return the index of the formatting string
     */
    def getFormatTypeIndex(s : String, argPos : reflect.Position) = {
      var i = 0
      val l = s.length
      if(l >= 1 && s.charAt(i) == '%') i += 1 
      else throw new TastyTypecheckError("too many arguments for interpolated string") //TODO : not a typecheck error and position

      while(i < l && isFlag(s.charAt(i))) {i += 1}
      while(i < l && Character.isDigit(s.charAt(i))) {i += 1}
      if(i < l && s.charAt(i) == '.') {
        i += 1
        while(i < l && Character.isDigit(s.charAt(i))) {i += 1}
      }
      if(i >= l) throw new TastyTypecheckError("Missing conversion operator in '" + s + "'; use %% for literal %, %n for newline") 
      i
    }
  
    // add the default "%s" format if no format is given by the user"
    val parts2 = strCtx.parts.toList match {
      case Nil => Nil
      case p :: parts1 => p :: parts1.map(part => if(!part.startsWith("%")) "%s" + part else part)
    }

    val format = parts2.size - 1
    val argument = args.size

    //TODO : should not be a typecheck error + add position
    if(format > argument && !(parts2.isEmpty && args.isEmpty)) throw new TastyTypecheckError("too few arguments for interpolated string")
    if (format < argument && !(parts2.isEmpty && args.isEmpty)) throw new TastyTypecheckError("too many arguments for interpolated string")
    if(parts2.isEmpty) throw new TastyTypecheckError("there are no parts")

    // typechecking 
    if(!parts2.isEmpty) {
      parts2.tail.zip(args.map(_.unseal)).foreach((part, arg) => { 
        val i = getFormatTypeIndex(part, arg.pos)
        part.charAt(i) match { 
            case 'c' | 'C' => 
              if(!checkSubtype(arg.tpe, definitions.CharType, definitions.ByteType, definitions.ShortType, definitions.IntType))
                throw new TastyTypecheckError("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Char\n") //TODO : position
            case 'd' | 'o' | 'x' | 'X' => 
              if (!checkSubtype(arg.tpe, definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger])){
                val conversionDetail = if(arg.tpe <:< definitions.StringType) { //TODO : when?
                  "Note that implicit conversions are not applicable because they are ambiguous:\n" + 
                  "both value strToInt2 of type String => Int\n" + 
                  "and value strToInt1 of type String => Int\n" +
                  "are possible conversion functions from String to Int\n"
                } else ""
                throw new TastyTypecheckError("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Int\n" + conversionDetail) //TODO : position
              }
            case 'e' | 'E' |'f' | 'g' | 'G' | 'a' | 'A' =>
              if (!checkSubtype(arg.tpe, definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal]))
                throw new TastyTypecheckError("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Double\n") //TODO : position
            case 't' | 'T' => 
              if (!checkSubtype(arg.tpe, definitions.LongType, typeOf[java.util.Calendar], typeOf[java.util.Date]))
                throw new TastyTypecheckError("type mismatch;\n found : " + arg.tpe.show + "\nrequired : \n") //TODO : add required + position
            case 'b' | 'B' => 
              if (!checkSubtype(arg.tpe, definitions.BooleanType, definitions.NullType))
                throw new TastyTypecheckError("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Boolean\n") //TODO : position
            case '%' | 'n' | 's' | 'S' | 'h' | 'H' => 
            case illegal => 
              throw new TastyTypecheckError("illegal conversion character '" + illegal + "'") //TODO : not a type check error 
        }
      })
    } 
      
    // macro expansion
    '((~parts2.mkString.toExpr).format(~args.toExprOfList: _*)) 
  }

  override protected def getStaticStringContext(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): StringContext = {
    import reflect._
    getStringContext(getListOfExpr(strCtxExpr))
  }

  /**
   * Transforms a given expression containing a StringContext into a list of expressions containing strings
   * @param strCtxExpr the given expression to convert
   * @throws NotNotStaticlyKnownError if the StringContext contained inside the given expression does not contain only
   * String literals
   * @return a list of expr of string corresponding to the parts of the given StringContext
   */
  protected def getListOfExpr(strCtxExpr : Expr[StringContext])(implicit reflect: Reflection): List[Expr[String]] = {
    import reflect._
    strCtxExpr.unseal.underlyingArgument match {
      case Term.Select(Term.Typed(Term.Apply(_, List(Term.Apply(_, List(Term.Typed(Term.Repeated(strCtxArgTrees, _), TypeTree.Inferred()))))), _), _) =>
        strCtxArgTrees.map(_.seal[String])
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
    }
  }

  /**
   * Computes the StringContext from a given list of expr containing strings
   * @param listExprStr the given list of expr of strings
   * @return the StringContext containing all the strings inside the given list
   */
  protected def getStringContext(listExprStr : List[Expr[String]])(implicit reflect: Reflection) : StringContext = {
    import reflect._
    val strings = listExprStr.map(_.unseal match {
      case Term.Literal(Constant.String(str)) => str
      case tree =>  throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
    })
    new StringContext(strings : _*)
  }
}

// TODO put this class in the stdlib or separate project?
abstract class MacroStringInterpolator[T] {

  final def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] = {
    try interpolate(strCtxExpr, argsExpr)
    catch {
      case ex: NotStaticlyKnownError =>
        // TODO use ex.expr to recover the position
        throw new QuoteError(ex.getMessage)
      case ex: StringContextError =>
        // TODO use ex.idx to recover the position
        throw new QuoteError(ex.getMessage)
      case ex: ArgumentError =>
        // TODO use ex.idx to recover the position
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
  protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] =
    interpolate(getStaticStringContext(strCtxExpr), getArgsList(argsExpr)) 

  /**
    * Interpolates the given arguments to the formatted string
    * @param strCtx that contains all the chunks of the formatted string
    * @param args the list of arguments to interpolate to the string in the correct format
    * @return the expression containing the formatted and interpolated string
    * @throws TastyTypecheckError if the given format is not correct  
    */
  protected def interpolate(strCtx: StringContext, argExprs: List[Expr[Any]])(implicit reflect: Reflection): Expr[T]

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
          case x :: xs => '(~x.toExpr :: ~toExpr(xs))
          case Nil => '(Nil)
        }
      }
      '(StringContext(~strCtx.parts.toList.toExpr: _*))
    }
  }

  protected class NotStaticlyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)
  protected class StringContextError(msg: String, idx: Int, start: Int = -1, end: Int = -1) extends Exception(msg)
  protected class ArgumentError(msg: String, idx: Int) extends Exception(msg)

}
