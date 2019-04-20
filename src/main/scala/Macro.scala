import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import reflect._

object Macro {
  implicit inline def (strCtx: => StringContext) f2(args: =>Any*): String = ${FIntepolator('strCtx, 'args)}
}

object FIntepolator extends MacroStringInterpolator[String] {

  /**
   * Computes the StringContext from a given list of expr containing strings
   * @param listExprStr the given list of expr of strings
   * @return the StringContext containing all the strings inside the given list
   * @throws NotStaticlyKnownError if the list of expr of strings does not contain only string literals
   */
  protected def getStringContext(listExprStr : List[Expr[String]])(implicit reflect: Reflection) : StringContext = {
    import reflect._
    val strings = listExprStr.map{case Literal(str : String) => str}
    new StringContext(strings : _*)
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
    strCtxExpr match {
      case '{ StringContext(${Repeated(parts)}: _*) } => 
        parts.map{case Literal(str : String) => str.seal.cast[String]}
      case _ =>
        List('{ "ERROR" })
    } 
  }

  override protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._

    /**
      * Adds the default "%s" to the strings that do not have any given format
      * @param parts the list of strings to add the default format to if neeeded
      * @return a new list of string with all a defined formatting 
      */
    def addDefaultFormat(partsExpr : List[Expr[String]]) : List[String] = {
       val parts : List[String] = partsExpr.map{case Literal(str : String) => str}
       parts match {
        case Nil => Nil
        case p :: parts1 => p :: parts1.zip(partsExpr.tail).map((part : String, partExpr : Expr[String]) => {
          if(!part.startsWith("%")) {
            val index = part.indexOf('%')
            if(index != -1) {
              val pos = partExpr.unseal.pos
              error("conversions must follow a splice; use %% for literal %, %n for newline", pos.sourceFile, pos.start + index, pos.start + index)
              "%s" + part
            } else "%s" + part 
          } else part
        })
      }
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
      if(relative) formatIndex += 1

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
          return curr :: getFlags(i + 1, l, s)
      }
      return Nil
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

    /**
      * Checks that a given part of the string context respects every formatting constraint
      * for every parameter
      * @param curr the argument that corresponds to the given part
      * @param args the list of arguments
      * @param part the given part of the string context
      * @param partExpr the expression containing the given part of the string context
      * @return an error if there has been a problem with any formatting parameter, nothing otherwise
      */
    def checkPart(curr : Expr[Any], args : List[Expr[Any]], part : String, partExpr : Expr[String], pos : Int) : Unit = {
      val arg = curr.unseal
      val argPosition = arg.pos
        val actualArgumentIndex : Int = args.indexOf(arg) + 1
        val (i, argument, argumentIndex, flags, width, widthIndex,precision, precisionIndex, relative) = getFormatTypeIndex(part, argPosition, false)
        val partPosition = partExpr.unseal.pos

        if(argument){
          if(relative)
            warning("Argument index ignored if '<' flag is present", partPosition.sourceFile, partPosition.start + argumentIndex + pos, partPosition.start + argumentIndex + pos)
          val usedArgumentIndex : Int = part.charAt(argumentIndex).asDigit

          if(usedArgumentIndex > args.size || usedArgumentIndex <= 0)
            error("Argument index out of range", partPosition.sourceFile, partPosition.start + argumentIndex + pos, partPosition.start + argumentIndex + pos)
          if(actualArgumentIndex != usedArgumentIndex)
            warning("Index is not this arg", partPosition.sourceFile, partPosition.start + argumentIndex + pos, partPosition.start + argumentIndex + pos)
        }

        part.charAt(i) match { 

            //Character

            case 'c' | 'C' => {
              if (!checkSubtype(arg.tpe, definitions.CharType, definitions.ByteType, definitions.ShortType, definitions.IntType))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Char\n", argPosition)
              for{flag <- flags ; if (flag != '-')} error("Only '-' allowed for c conversion", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            //Integral

            case 'd' => {
              if (!checkSubtype(arg.tpe, definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger]))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Int\n", argPosition)
              for{flag <- flags} flag match {
                case '#' => error("# not allowed for d conversion", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
                case _ => //OK
              }
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            case 'o' | 'x' | 'X' => {
              if (!checkSubtype(arg.tpe, definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger]))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Int\n", argPosition)
              for{flag <- flags} flag match {
                case '+' | ' ' | '(' if(!checkSubtype(arg.tpe, typeOf[java.math.BigInteger])) => error("Only use '" + flag + "' for BigInt conversions to o, x, X", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
                case ',' => error("',' only allowed for d conversion of integral types", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
                case _ => //OK
              }
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            //Floating points

            case 'e' | 'E' |'f' | 'g' | 'G' =>
              if (!checkSubtype(arg.tpe, definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal]))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Double\n", argPosition)
            case 'a' | 'A' => {
              if (!checkSubtype(arg.tpe, definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal]))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Double\n", argPosition)
              for{flag <- flags ; if (flag == ',' || flag == '(')} error("'" + flag + "' not allowed for a, A", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            }

            //Date/time

            case 't' | 'T' => {
              if (!checkSubtype(arg.tpe, definitions.LongType, typeOf[java.util.Calendar], typeOf[java.util.Date]))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Date\n", argPosition)
              for{flag <- flags ; if flag != '-'} error("Only '-' allowed for date/time conversions", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
              if(i == part.size - 1) error("Date/time conversion must have two characters", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              
              val suffix = part.charAt(i + 1)
              checkTime(suffix, partPosition, i + 1)
            }

            //General

            case 'b' | 'B' => {
              if (!checkSubtype(arg.tpe, definitions.BooleanType, definitions.NullType))
                error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : Boolean\n", argPosition)
              for{flag <- flags ; if (flag != '-')} error("Illegal flag : '" + flag + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
            }
            case 'h' | 'H' => 
              for{flag <- flags ; if (flag != '-')} error("Illegal flag : '" + flag + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
            case 's' | 'S' => 
              for{flag <- flags ; if (flag != '-')} {
                if(flag == '#' && !checkSubtype(typeOf[java.util.Formattable])) error("type mismatch;\n found : " + arg.tpe.show + "\nrequired : java.util.Formattable\n", argPosition)
                else error("Illegal flag : '" + flag + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
              }
            case 'n' if(width) => error("width not allowed", partPosition.sourceFile, partPosition.start + widthIndex, partPosition.start + widthIndex)
            case 'n' | '%' => if(precision) error("precision not allowed", partPosition.sourceFile, partPosition.start + precisionIndex, partPosition.start + precisionIndex)
            case illegal => 
              val partPos = partExpr.unseal.pos
              error("illegal conversion character '" + illegal + "'", partPosition.sourceFile, partPosition.start + i, partPosition.start + i)
        }

        val (next, start) = newPart(part, i)
        if(next != null) checkPart(curr, args, next, partExpr, i + start) 
    } 

    def newPart(string : String, index : Int) : (String, Int) = {
      val substring = string.substring(index)
      val size = substring.size
      var i = 0
      while (i < size){
        if(substring.charAt(i) == '%'){
          val start = i
          i += 1
          while(i < size && substring.charAt(i).isDigit) i += 1
          if(substring.charAt(i) == '$') return (substring.substring(start), start)
        }
        i += 1
      }
      (null, 0)
    }

    val partsExpr = getListOfExpr(strCtxExpr)
    val args = getArgsList(argsExpr)
    
    val argument = args.size
    val argPos = if(args.isEmpty) argsExpr.unseal.pos else args(argument - 1).unseal.underlyingArgument.pos 
    val strPos = strCtxExpr.unseal.underlyingArgument.pos 
    checkSizes(partsExpr.size - 1, argument, argPos, strPos)

    val parts = addDefaultFormat(partsExpr)

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

      val zippedd = (parts.tail, args, partsExpr.tail).zipped
      for {(part, arg, partExpr) <- zippedd} checkPart(arg, args, part, partExpr, 0)
    } 
      
    // macro expansion
    '{(${parts.mkString.toExpr}).format(${args.toExprOfList}: _*)}
  }
}