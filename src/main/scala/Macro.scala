import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

object Macro {
  implicit inline def (strCtx: => StringContext) f2(args: =>Any*): String = ${FIntepolator('strCtx, 'args)}
}

object FIntepolator extends MacroStringInterpolator[String] {
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
      case Term.Apply(Term.Select(Term.Select(_, "StringContext"), "apply"),
        List(Term.Typed(Term.Repeated(strCtxArgTrees, _), _))) =>
        strCtxArgTrees.map(_.seal[String])
      case Term.Apply(Term.Select(Term.New(TypeTree.Ident("StringContext")), "<init>"), 
        List(Term.Typed(Term.Repeated(strCtxArgTrees, _), _))) => 
        strCtxArgTrees.map(_.seal[String])
      case Term.Apply(Term.Select(Term.Ident("StringContext"), "apply"), 
        List(Term.Typed(Term.Repeated(strCtxArgTrees, _), _))) => 
        strCtxArgTrees.map(_.seal[String])
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext " + tree.show, tree.seal[Any])
    }
  }

  /**
   * Computes the StringContext from a given list of expr containing strings
   * @param listExprStr the given list of expr of strings
   * @return the StringContext containing all the strings inside the given list
   * @throws NotStaticlyKnownError if the list of expr of strings does not contain only string literals
   */
  protected def getStringContext(listExprStr : List[Expr[String]])(implicit reflect: Reflection) : StringContext = {
    import reflect._
    val strings = listExprStr.map(stringExprToString)
    new StringContext(strings : _*)
  }

  /**
   * Computes the String from a given expr containing a string
   * @param stringExpr the given expr of string
   * @return the String contained in the given expr
   * @throws NotStaticlyKnownError if the given expr does not contain a string literal
   */
  protected def stringExprToString(stringExpr : Expr[String])(implicit reflect : Reflection) : String = {
    import reflect._
    stringExpr.unseal match {
      case Term.Literal(Constant.String(str)) => str
      case tree =>  throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal[Any])
    }
  }

  override protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    /**
      * Adds the default "%s" to the strings that do not have any given format
      * @param parts the list of strings to add the default format to if neeeded
      * @return a new list of string with all a defined formatting 
      */
    def addDefaultFormat(parts : List[String]) : List[String] = parts match {
      case Nil => Nil
      case p :: parts1 => p :: parts1.map(part => if(!part.startsWith("%")) "%s" + part else part)
    }

    /**
     * Checks if the number of arguments are the same as the number of formatting strings
     */
    def checkSizes(format : Int, argument : Int, argPos : Position, strPos : Position) = {
      if(format > argument && !(format == -1 && argument == 0))
        error("too few arguments for interpolated string", argPos)
      if (format < argument && !(format == -1 && argument == 0)) 
        error("too many arguments for interpolated string", argPos) 
      if(format == -1) error("there are no parts", strPos.sourceFile, strPos.start, strPos.start + 1)
    } 

    /**
     * Go through the whole given string until it find the formatting string 
     * and returns the corresponding index
     * @param s the given string containing the formatting string as substring
     * @param argPos the position of the argument to format, only useful to throw errors
     * @param noArg true if there is no arg, i.e. "%%" or "%n" 
     * @throws TastyTypecheckError if the formatting string has not the correct format
     * @return the index of the formatting (conversion) string and 
     * a list of potential flags (Some(flag) if it is, None otherwise) and
     * true if there is a width parameter (with the corresponding index of the width parameter), false otherwise and 
     * true if there is a precision parameter  (with the corresponding index of the precision parameter), false otherwise
     */
    def getFormatTypeIndex(s : String, argPos : reflect.Position, noArg : Boolean) : (Int, Boolean, Int, List[Char], Boolean, Int, Boolean, Int, Boolean) = {
      var formatIndex = 0 
      var argument = false
      var argumentIndex = 0
      var precision = false 
      var precisionIndex = 0
      var width = false 
      var widthIndex = 0
      val l = s.length
      
      if(l >= 1 && s.charAt(formatIndex) == '%') formatIndex += 1 
      else if(!noArg) error("too many arguments for interpolated string", argPos)

      //argument index or width
      while(formatIndex < l && Character.isDigit(s.charAt(formatIndex))){
        width = true
        widthIndex = formatIndex
        formatIndex += 1
      }

      //argument index
      if(formatIndex < l && s.charAt(formatIndex) == '$'){
        width = false
        argument = true
        argumentIndex = widthIndex
        formatIndex += 1
      }

      //relative indexing
      val relative = s.charAt(formatIndex) == '<'

      //flags

      val flags = getFlags(formatIndex, l, s)
      formatIndex += flags.size

      //width
      while(formatIndex < l && Character.isDigit(s.charAt(formatIndex))) {
        width = true
        widthIndex = formatIndex
        formatIndex += 1
      }

      //precision
      if(formatIndex < l && s.charAt(formatIndex) == '.') {
        precisionIndex = formatIndex
        formatIndex += 1
        precision = true
        while(formatIndex < l && Character.isDigit(s.charAt(formatIndex))) {formatIndex += 1}
      }
      if(formatIndex >= l) error("Missing conversion operator in '" + s + "'; use %% for literal %, %n for newline", argPos)

      (formatIndex, argument, argumentIndex, flags, width, widthIndex, precision, precisionIndex, relative)
    }

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
      * Finds all the flags that are inside a given string from a given index
      * @param i the index in the string s where to start to analyse the string
      * @param l the length of the given string
      * @param s the given string to analyse
      * @return a list containing all the flags that are inside the given string
      */
    def getFlags(i : Int, l : Int, s : String) : List[Char] = {
      if(i < l) {
        val curr = s.charAt(i)
        if(isFlag(curr))
          curr :: getFlags(i + 1, l, s)
      } 
      Nil
    }

    /**
     * Checks that the suffix for time conversions exists and is correct
     * @param suffix the given suffix to check
     * @param position the position where the error will be
     * @param offset the number to offset the position so that the error is very precise
     * @return an error if this is not the case, nothing otherwise
     */
    def checkTime(suffix : Char, position : Position, offset : Int) : Unit = suffix match {
      case 'H' | 'I' | 'k' | 'l' | 'M' | 'S' | 'L' | 'N' | 'p' | 'z' | 'Z' | 's' | 'Q' => //times
      case 'B' | 'b' | 'h' | 'A' | 'a' | 'C' | 'Y' | 'y' | 'j' | 'm' | 'd' | 'e' => //dates
      case 'R' | 'T' | 'r' | 'D' | 'F' | 'c' => //dates and times
      case c => error("'" + c + "' doesn't seem to be a date or time conversion", position.sourceFile, position.start + offset, position.start + offset)
    }

    val partsExpr = getListOfExpr(strCtxExpr)
    val args = getArgsList(argsExpr)

    val parts = addDefaultFormat(partsExpr.map(stringExprToString))
    
    val argument = args.size
    val argPos = if(args.isEmpty) argsExpr.unseal.pos else args(argument - 1).unseal.underlyingArgument.pos 
    val strPos = strCtxExpr.unseal.underlyingArgument.pos 
    checkSizes(parts.size - 1, argument, argPos, strPos)

    // formatting parameters checking
    if(!parts.isEmpty) {
      if(parts.size == 1 && args.size == 0 && parts.head.size != 0){
        val position = partsExpr.head.unseal.pos
        val (i, argument, argumentIndex, _, width, widthIndex, precision, precisionIndex, relative) = getFormatTypeIndex(parts.head, position, true)

        if(argument)
          error("Argument index cannot be used if no argument is given", position.sourceFile, position.start + argumentIndex + 1, position.start + argumentIndex + 1)

        if(relative)
          error("No last arg", position.sourceFile, position.start + i + 1, position.start + i + 1)

        parts.head.charAt(i) match {
          case 'n' if(width) => error("width not allowed", position.sourceFile, position.start + widthIndex, position.start + widthIndex)
          case 'n' | '%' => if(precision) error("precision not allowed", position.sourceFile, position.start + precisionIndex + 1, position.start + precisionIndex + 1) 
          case illegal => 
        }
      }

      val zippedd = (parts.tail, args.map(_.unseal), partsExpr.tail).zipped
      for {(part, arg, partExpr) <- zippedd} {
        val argPosition = arg.pos
        val actualArgumentIndex : Int = args.indexOf(arg) + 1
        val (i, argument, argumentIndex, flags, width, widthIndex,precision, precisionIndex, relative) = getFormatTypeIndex(part, argPosition, false)
        val partPosition = partExpr.unseal.pos

        if(argument){
          if(relative)
            warning("Argument index ignored if '<' flag is present", partPosition.sourceFile, partPosition.start + argumentIndex, partPosition.start + argumentIndex)
          val usedArgumentIndex : Int = part.charAt(argumentIndex).asDigit
          if(actualArgumentIndex != usedArgumentIndex)
            warning("Index is not this arg", partPosition.sourceFile, partPosition.start + argumentIndex, partPosition.start + argumentIndex)
          if(usedArgumentIndex > args.size || usedArgumentIndex <= 0)
            error("Argument index out of range", partPosition.sourceFile, partPosition.start + argumentIndex, partPosition.start + argumentIndex)
        //TODO : problem : the string "${8}%d ${9}%d%3$$d" becomes StringContext("%d", "%d%3$d") => no index out of range
        //TODO : problem : the string "$s%s $s%s %1$$<s" becomes StringContext("%s", "%s %1$<s") => no argument ignored
        }

        part.charAt(i) match { 

            //Character

            case 'c' | 'C' => {
              if (!checkSubtype(arg.tpe, definitions.CharType, definitions.ByteType, definitions.ShortType, definitions.IntType))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Char\n", argPosition)
              for{flag <- flags ; if (flag != '-')} error("Only '-' allowed for c conversion", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            //Integral

            case 'd' => {
              if (!checkSubtype(arg.tpe, definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger]))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Int\n", argPosition)
              for{flag <- flags} flag match {
                case '#' => error("# not allowed for d conversion", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
                case _ => //OK
              }
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            case 'o' | 'x' | 'X' => {
              if (!checkSubtype(arg.tpe, definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger]))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Int\n", argPosition)
              for{flag <- flags} flag match {
                case '+' | ' ' | '(' if(!checkSubtype(arg.tpe, typeOf[java.math.BigInteger])) => error("only use '" + flag + "' for BigInt conversions to o, x, X", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
                case ',' => error("',' only allowed for d conversion of integral types", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
                case _ => //OK
              }
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            //Floating points

            case 'e' | 'E' |'f' | 'g' | 'G' =>
              if (!checkSubtype(arg.tpe, definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal]))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Double\n", argPosition)
            case 'a' | 'A' => {
              if (!checkSubtype(arg.tpe, definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal]))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Double\n", argPosition)
              for{flag <- flags ; if (flag == ',' || flag == '(')} error("'" + flag + "' not allowed for a, A", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            //Date/time

            case 't' | 'T' => {
              if (!checkSubtype(arg.tpe, definitions.LongType, typeOf[java.util.Calendar], typeOf[java.util.Date]))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Date\n", argPosition)
              for{flag <- flags ; if flag == '-'} error("Only '-' allowed for date/time conversions", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
              if(i == part.size - 1) error("Date/time conversion must have two characters", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              
              val suffix = part.charAt(i + 1)
              checkTime(suffix, partPosition, i + 1)
            }

            //General

            case 'b' | 'B' => {
              if (!checkSubtype(arg.tpe, definitions.BooleanType, definitions.NullType))
                error("type mismatch;\n found : " + arg.tpe.showCode + "\nrequired : Boolean\n", argPosition)
              for{flag <- flags ; if (flag != '-' || flag != '#')} error("Illegal flag : '" + flag + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
            }
            case 'h' | 'H' | 's' | 'S' => 
              for{flag <- flags ; if (flag != '-' || flag != '#')} error("Illegal flag : '" + flag + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
            
            case 'n' if(width) => error("width not allowed", partPosition.sourceFile, partPosition.start + widthIndex, partPosition.start + widthIndex)
            case 'n' | '%' => if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            case illegal => 
              val partPos = partExpr.unseal.pos
              error("illegal conversion character '" + illegal + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
        }
      }
    } 
      
    // macro expansion
    '{(${parts.mkString.toExpr}).format(${args.toExprOfList}: _*)}
  }
}