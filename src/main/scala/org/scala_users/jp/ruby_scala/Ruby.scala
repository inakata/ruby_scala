package org.scala_users.jp.ruby_scala
import scala.util.parsing.combinator._
import scala.util.parsing.input._

class Ruby extends RegexParsers with PackratParsers with TracableParsers {

  override def skipWhitespace = false

  def ??(p: Parser[String]): Parser[String] = opt(p) ^^ {
       case None => ""
       case Some(a) => a.toString
  }
  def r1chain[T, U](p: Parser[T], q: Parser[U])(combiner: (T, U) => T): Parser[T] =  p ~ rep(q) ^^ {
    case x ~ xs => xs.foldLeft(x){ case (a, b) => combiner(a, b) }
  }

  lazy val anySpace: Parser[String] = ((regex("""\s""".r) | Comment)*) ^^ { (_).mkString }
  lazy val anySpaceWithoutLT: Parser[String] = regex("""[ \t\f]*""".r) ~ ??(Comment) ^^ { case r~c => r+c }
  lazy val WS: Parser[String] = regex("""\s+""".r)
  lazy val ConstID: Parser[String] = anySpace~>regex("""[A-Z][a-zA-Z0-9_]*""".r)
  lazy val LocalID: Parser[String] = anySpace~>regex("""[a-z_][a-zA-Z0-9_]*""".r)
  lazy val MethodID: Parser[String] = anySpace~>regex("""m[a-zA-Z0-9_]*""".r)
  lazy val GlobalID: Parser[String] = anySpace~>regex("""\$[a-zA-Z_][a-zA-Z0-9_]*""".r)
  lazy val ClassID: Parser[String] = anySpace~>regex("""@@[a-zA-Z_][a-zA-Z0-9_]*""".r)
  lazy val InstanceID: Parser[String] = anySpace~>regex("""@[a-zA-Z_][a-zA-Z0-9_]*""".r)

// 8.2 Program text
  lazy val SourceCharacter: Parser[String] =
             regex("""[\u0000-\u007f]""".r)
// 8.3 Line terminators
  lazy val LineTerminator: Parser[String] =
             anySpaceWithoutLT~>regex("""\u000a|\u000d\u000a""".r) //^^ { case a => " EOL\n" }
// 8.4 Whitespace
  lazy val Whitespace: Parser[String] =
             ("\u0009" | "\u000b" | "\u000c" | "\u000d" | "\u0020" ) |
             LineTerminatorEscapeSequence
  lazy val LineTerminatorEscapeSequence: Parser[String] =
             "\\" ~ LineTerminator ^^ { case s~l => " escaped-EOL\n" }

  lazy val IdentifierCharacter: Parser[String] = regex("""[a-zA-Z0-9_]""".r)
  lazy val UppercaseCharacter: Parser[String] = regex("""[A-Z]""".r)
  lazy val LowercaseCharacter: Parser[String] = regex("""[a-z]""".r)

  lazy val SingleQUOTE: Parser[String] = anySpace~>"'"
  lazy val MULT: Parser[String] = anySpace~>"*"
  lazy val DIV: Parser[String] = anySpace~>"/"
  lazy val LPAREN: Parser[String] = anySpace~>"("
  lazy val RPAREN: Parser[String] = anySpace~>")"
  lazy val LBRACE: Parser[String] = anySpace~>"{"
  lazy val RBRACE: Parser[String] = anySpace~>"}"
  lazy val LBRACKET: Parser[String] = anySpace~>"["
  lazy val RBRACKET: Parser[String] = anySpace~>"]"
  lazy val QUESTION: Parser[String] = anySpace~>"?"
  lazy val COMMA: Parser[String] = anySpace~>","
  lazy val VerticalBAR: Parser[String] = anySpace~>"|"
  lazy val SEMICOLON: Parser[String] = anySpace~>";"
  lazy val COLON: Parser[String] = anySpace~>":"
  lazy val DoubleCOLON: Parser[String] = anySpace~>"::"
  lazy val PERIOD: Parser[String] = anySpace~>"."
  lazy val DoublePERIOD: Parser[String] = anySpace~>".."
  lazy val TriplePERIOD: Parser[String] = anySpace~>"..."
  lazy val AndOP: Parser[String] = anySpace~>"&&"
  lazy val OrOP: Parser[String] = anySpace~>"||"
  lazy val NotEQ: Parser[String] = anySpace~>"!="
  lazy val NotOP: Parser[String] = anySpace~>"!"
  lazy val NotTILDE: Parser[String] = anySpace~>"!~"
  lazy val AMPERSAND: Parser[String] = anySpace~>"&"

  lazy val ArrayEQ: Parser[String] =  anySpace~>"[]="
  lazy val ArraySYM: Parser[String] =  anySpace~>"[]"
  lazy val AndASSIGN: Parser[String] =  anySpace~>"&&="
  lazy val OrASSIGN: Parser[String] =  anySpace~>"||="
  lazy val HatASSIGN: Parser[String] =  anySpace~>"^="
  lazy val AmpersandASSIGN: Parser[String] =  anySpace~>"&="
  lazy val VerticalBarASSIGN: Parser[String] =  anySpace~>"|="
  lazy val LtLtASSIGN: Parser[String] =  anySpace~>"<<="
  lazy val GtGtASSIGN: Parser[String] =  anySpace~>">>="
  lazy val PlusASSIGN: Parser[String] =  anySpace~>"+="
  lazy val MinusASSIGN: Parser[String] =  anySpace~>"-="
  lazy val StarStarASSIGN: Parser[String] =  anySpace~>"**="
  lazy val StarASSIGN: Parser[String] =  anySpace~>"*="
  lazy val SlashASSIGN: Parser[String] =  anySpace~>"/="
  lazy val PercentASSIGN: Parser[String] =  anySpace~>"%="

  lazy val HAT: Parser[String] =  anySpace~>"^"
  lazy val LtEqGT: Parser[String] =  anySpace~>"<=>"
  lazy val TripleEQ: Parser[String] =  anySpace~>"==="
  lazy val DoubleEQ: Parser[String] =  anySpace~>"=="
  lazy val EqTILDE: Parser[String] =  anySpace~>"=~"
  lazy val EqGT: Parser[String] =  anySpace~>"=>"
  lazy val GtEQ: Parser[String] =  anySpace~>">="
  lazy val GtGT: Parser[String] =  anySpace~>">>"
  lazy val GT: Parser[String] =  anySpace~>">"
  lazy val LtLT: Parser[String] =  anySpace~>"<<"
  lazy val LtEQ: Parser[String] =  anySpace~>"<="
  lazy val LT: Parser[String] =  anySpace~>"<"
  lazy val EQUAL: Parser[String] =  anySpace~>"="
  lazy val PLUS: Parser[String] =  anySpace~>"+"
  lazy val PlusAT: Parser[String] =  anySpace~>"+@"
  lazy val MinusAT: Parser[String] =  anySpace~>"-@"
  lazy val MINUS: Parser[String] =  anySpace~>"-"
  lazy val StarSTAR: Parser[String] =  anySpace~>"**"
  lazy val STAR: Parser[String] =  anySpace~>"*"
  lazy val SLASH: Parser[String] =  anySpace~>"/"
  lazy val PERCENT: Parser[String] =  anySpace~>"%"
  lazy val TILDE: Parser[String] =  anySpace~>"~"
  lazy val BackQUOTE: Parser[String] =  anySpace~>"`"

  lazy val AND: Parser[String] = anySpace~>"and"
  lazy val OR: Parser[String] = anySpace~>"or"
  lazy val NOT: Parser[String] = anySpace~>"not"
  lazy val RETURN: Parser[String] = anySpace~>"return"
  lazy val BREAK: Parser[String] = anySpace~>"break"
  lazy val NEXT: Parser[String] = anySpace~>"next"
  lazy val SUPER: Parser[String] = anySpace~>"super"
  lazy val YIELD: Parser[String] = anySpace~>"yield"
  lazy val DO: Parser[String] = anySpace~>"do"
  lazy val END: Parser[String] = anySpace~>"end"
  lazy val LINE: Parser[String] = anySpace~>"__LINE__"
  lazy val ENCODING: Parser[String] = anySpace~>"__ENCODING__"
  lazy val FILE: Parser[String] = anySpace~>"__FILE__"
  lazy val UpBEGIN: Parser[String] = anySpace~>"BEGIN"
  lazy val UpEND: Parser[String] = anySpace~>"END"
  lazy val ALIAS: Parser[String] = anySpace~>"alias"
  lazy val BEGIN: Parser[String] = anySpace~>"begin"
  lazy val CASE: Parser[String] = anySpace~>"case"
  lazy val CLASS: Parser[String] = anySpace~>"class"
  lazy val DEF: Parser[String] = anySpace~>"def"
  lazy val DEFINED: Parser[String] = anySpace~>"defined?"
  lazy val ELSE: Parser[String] = anySpace~>"else"
  lazy val ELSIF: Parser[String] = anySpace~>"elsif"
  lazy val ENSURE: Parser[String] = anySpace~>"ensure"
  lazy val FOR: Parser[String] = anySpace~>"for"
  lazy val FALSE: Parser[String] = anySpace~>"false"
  lazy val IF: Parser[String] = anySpace~>"if"
  lazy val IN: Parser[String] = anySpace~>"in"
  lazy val MODULE: Parser[String] = anySpace~>"module"
  lazy val NIL: Parser[String] = anySpace~>"nil"
  lazy val REDO: Parser[String] = anySpace~>"redo"
  lazy val RESCUE: Parser[String] = anySpace~>"rescue"
  lazy val RETRY: Parser[String] = anySpace~>"retry"
  lazy val SELF: Parser[String] = anySpace~>"self"
  lazy val THEN: Parser[String] = anySpace~>"then"
  lazy val TRUE: Parser[String] = anySpace~>"true"
  lazy val UNDEF: Parser[String] = anySpace~>"undef"
  lazy val UNLESS: Parser[String] = anySpace~>"unless"
  lazy val UNTIL: Parser[String] = anySpace~>"until"
  lazy val WHEN: Parser[String] = anySpace~>"when"
  lazy val WHILE: Parser[String] = anySpace~>"while"

// 10.1, 10.2,

  lazy val program: Parser[Any] =
             compoundStatement
  lazy val compoundStatement: Parser[String] =
             memo(??(statementList) <~ ??(separatorList))
  lazy val statementList: Parser[String] =
             memo(rep1sep(statement, separatorList) ^^ { (_).mkString("", "\n", "") } )
  lazy val separatorList: Parser[String] =
             ( separator+ ) ^^ { case e => ";" }
  lazy val separator: Parser[String] =
             LineTerminator |
             SEMICOLON


// 11.1

  lazy val expression: Parser[String] = // notExpression | keywordAndExpression | keywordOrExpression
             memo(chainl1 ( notExpression, not(LineTerminator) ~ (AND | OR) ^^ {
                            case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } ))
  lazy val notExpression: Parser[String] = memo(
             keywordNotExpression |
             NotOP ~ methodInvocationWithoutParentheses ^^ {case n~m => "!" + m } |
             methodInvocationWithoutParentheses |
             operatorExpression )

  lazy val keywordNotExpression: Parser[String] =
             NOT ~ notExpression ^^ { case n~e => "(not " +e+ ")" }

// 11.2.3 Logical AND expressions

  lazy val operatorAndExpression: Parser[String] = memo(
             chainl1 ( equalityExpression, not(LineTerminator) ~ AndOP ^^ {
                             case n~o => (l: String, r: String) => "("+l+" "+o+" "+r+")" } ))

// 11.2.4 Logical OR expressions

  lazy val operatorOrExpression: Parser[String] = memo(
             chainl1 ( operatorAndExpression, not(LineTerminator) ~ OrOP ^^ {
                             case n~o => (l: String, r: String) => "("+l+" "+o+" "+r+")" } ))

// 11.3.1

  lazy val primaryMethodInvocation: Parser[String] = memo(
             superWithOptionalArgument |
             MethodOnlyIdentifier |
             MethodIdentifier ~ block ^^ { case m~b => m + b } |
             MethodIdentifier ~ not(WS) ~ argumentWithParentheses ~ ??(block) ^^ {
                             case m~n~a~b => m + a + b } )
  lazy val MethodIdentifier: Parser[String] =
             MethodID |
             ConstantIdentifier |
             MethodOnlyIdentifier // not LocalVariableIdentifier
  lazy val methodName: Parser[String] =
             OperatorMethodName |
             MethodIdentifier |
             Keyword
// lazy val indexing-method-invocation // in primaryExpression
  lazy val methodNameExceptConstant: Parser[String] =
             not(ConstID) ~ methodName ^^ { case n~m => m }
  lazy val methodInvocationWithoutParentheses: Parser[String] = memo(
             command ^^ { case c => "method-inv-without-par("+c+")" } |
             chainedCommandWithDoBlock ~ (PERIOD | DoubleCOLON) ~ methodName ~ argumentWithoutParentheses ^^ {
                              case c~p~m~a => "method-inv-without-par("+c+p+m+" "+a+")" } |
             chainedCommandWithDoBlock ^^ { case c => "method-inv-without-par("+c+")" } |
             returnWithArgument |
             breakWithArgument |
             nextWithArgument )
  lazy val command: Parser[String] = memo(
             superWithArgument |
             yieldWithArgument |
             MethodIdentifier ~ not(not(WS) ~ LPAREN) ~ argumentWithoutParentheses ^^ {
                              case m~n~a => "command("+m+" "+a+")" } |  //
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ methodName ~ not(not(WS) ~ LPAREN) ~
                 argumentWithoutParentheses ^^ { case p~n1~c~m~n2~a => "command("+p+c+m+" "+a+")" } )
  lazy val chainedCommandWithDoBlock: Parser[String] = memo(
             r1chain ( commandWithDoBlock, chainedMethodInvocation )  { case (x, y) => "chained(" + x + y + ")" } )
  lazy val chainedMethodInvocation: Parser[String] =
             (PERIOD | DoubleCOLON) ~ methodName ~ ??(argumentWithParentheses) ^^ { case p~m~a => p+m+a }
  lazy val commandWithDoBlock: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ methodName ~
                 argumentWithoutParentheses ~ doBlock ^^ { case p~n~c~m~a~d => "command-do("+p+c+m+" "+a+" "+d+")" } |
             MethodIdentifier ~ argumentWithoutParentheses ~ doBlock ^^ {
                              case m~a~d => "command-do("+m+" "+a+" "+d+")" } |
             superWithArgumentAndDoBlock

//11.3.2 Method arguments

  lazy val indexingArgumentList: Parser[String] =
             command |
             operatorExpressionList ~ not(LineTerminator) ~ COMMA ~ splattingArgument ^^ {
                              case o~n~c~s => o+", "+s } |
             operatorExpressionList ~ ??( not(LineTerminator) ~ COMMA ^^ { case n~c => ", " } ) ^^ {
                              case o~c => o+c } |
             associationList ~ ??( not(LineTerminator) ~ COMMA ^^ { case n~c => ", " } ) ^^ { case a~c => a+c } |
             splattingArgument
  lazy val splattingArgument: Parser[String] =
             STAR ~ operatorExpression ^^ { case s~o => "*"+o }
  lazy val operatorExpressionList: Parser[String] = memo(
             rep1sep(operatorExpression, not(LineTerminator) ~ COMMA) ^^ { (_).mkString("", ", ", "")  }  )
  lazy val argumentWithParentheses: Parser[String] = not(WS) ~ parenthesesAndArgument ^^ { case n~p => p }
  lazy val parenthesesAndArgument: Parser[String] =
             LPAREN ~ RPAREN ^^ { case l~r => "( )" } |
             LPAREN ~ argumentList ~ RPAREN ^^ { case l~a~r => "("+a+")" } |
             LPAREN ~ operatorExpressionList ~ not(LineTerminator) ~ COMMA ~ chainedCommandWithDoBlock ~ RPAREN ^^ {
                               case l~o~n~c~b~r => "("+o+","+b+")" } |
             LPAREN ~ chainedCommandWithDoBlock ~ RPAREN ^^ { case l~b~r => "("+b+")" }
  lazy val argumentWithoutParentheses: Parser[String] =
             not(LBRACE) ~ not(LineTerminator) ~ argumentList ^^ { case n1~n2~a => a }
  lazy val argumentList: Parser[String] =
             command |
           blockArgument |
           splattingArgument ~ ??( COMMA ~ blockArgument  ^^ { case c~b => ", "+b } ) ^^ { case s~c => s+c } |
           operatorExpressionList ~ not(LineTerminator) ~ COMMA ~ associationList ~
              ??( not(LineTerminator) ~ COMMA ~ splattingArgument ^^ { case n~c~s => ", "+s }) ~
              ??( not(LineTerminator) ~ COMMA ~ blockArgument ^^ { case n~c~b => ", "+b } )  ^^ {
                               case o~n~c~a~s~b => o+", "+a+s+b } |
           ( operatorExpressionList | associationList ) ~
              ??( not(LineTerminator) ~ COMMA ~ splattingArgument ^^ { case n~c~s => ", "+s }) ~
              ??( not(LineTerminator) ~ COMMA ~ blockArgument ^^ { case n~c~b => ", "+b } )  ^^ {
                               case oa~s~a => oa+s+a }
  lazy val blockArgument: Parser[String] =
             AMPERSAND ~ operatorExpression ^^ { case a~o => "&"+o }

// 11.3.3 Blocks

  lazy val block: Parser[String] = memo(
             braceBlock |
             doBlock )
  lazy val braceBlock: Parser[String] =
             LBRACE ~ ??(blockParameter) ~ blockBody ~ RBRACE ^^ { case l~p~b~r => "{"+p+b+"}" }
  lazy val doBlock: Parser[String] =
             DO ~ ??(blockParameter) ~ blockBody ~ END ^^ { case d~p~b~e => " do "+p+b+" end" }
  lazy val blockParameter: Parser[String] =
             VerticalBAR ~ VerticalBAR ^^ { case v1~v2 => "| | " } |
             OrOP |
             VerticalBAR ~ blockParameterList ~ VerticalBAR ^^ { case v1~b~v2 => "|"+b+"| " }
  lazy val blockParameterList: Parser[String] =
             leftHandSide |
             multipleLeftHandSide
  lazy val blockBody: Parser[String] =
             compoundStatement

// 11.3.4 The super expression

  lazy val superWithOptionalArgument: Parser[String] =
             SUPER ~ ??(not(WS) ~ argumentWithoutParentheses ^^ { case n~a => a } ) ~
                 ??(block) ^^ { case s~a~b => "(super "+a+b+")" }
  lazy val superWithArgument: Parser[String] =
             SUPER ~ argumentWithoutParentheses ^^ { case s~a => "(super "+a+")" }
  lazy val superWithArgumentAndDoBlock: Parser[String] =
             SUPER ~ argumentWithoutParentheses ~ doBlock ^^ { case s~a~b => "(super "+a+b+")" }

// 11.3.5 The yield expression

  lazy val yieldWithOptionalArgument: Parser[String] =
             yieldWithParenthesesAndArgument |
             yieldWithParenthesesWithoutArgument |
             YIELD
  lazy val yieldWithParenthesesAndArgument: Parser[String] =
             YIELD ~ not(WS) ~ LPAREN ~ argumentList ~ RPAREN ^^ { case y~n~l~a~r => "yield ("+a+")" }
  lazy val yieldWithParenthesesWithoutArgument: Parser[String] =
             YIELD ~ not(WS) ~ LPAREN ~ RPAREN ^^ { case y~n~l~r => "yield ( )" }
  lazy val yieldWithArgument: Parser[String] =
             YIELD ~ argumentWithoutParentheses ^^ { case y~a => "(yield "+a+")" }


// 11.4 Operator expressions, 11.4.1

  lazy val operatorExpression: Parser[String] = memo(
             assignmentExpression |
             definedWithoutParentheses |
             conditionalOperatorExpression // ^^ { case c => "op-exp("+c+")" }
             )
// 11.4.2 Assignments

  lazy val assignmentExpression: Parser[String] =
             singleAssignmentExpression |
             abbreviatedAssignmentExpression |
             assignmentWithRescueModifier
  lazy val assignmentStatement: Parser[String] =
             singleAssignmentStatement |
             abbreviatedAssignmentStatement |
             multipleAssignmentStatement

// 11.4.2.2 Single assignments

  lazy val singleAssignmentExpression: Parser[String] =
             singleVariableAssignmentExpression |
             scopedConstantAssignmentExpression |
             singleIndexingAssignmentExpression |
             singleMethodAssignmentExpression
  lazy val singleAssignmentStatement: Parser[String] =
             singleVariableAssignmentStatement |
             scopedConstantAssignmentStatement |
             singleIndexingAssignmentStatement |
             singleMethodAssignmentStatement

// 11.4.2.2.2 Single variable assignments

  lazy val singleVariableAssignmentStatement: Parser[String] =
             variable ~ not(LineTerminator) ~ EQUAL ~ methodInvocationWithoutParentheses ^^ {
                           case v~n~e~m => "single-variable-st("+v+"="+m+")" }
  lazy val singleVariableAssignmentExpression: Parser[String] =
             variable ~ not(LineTerminator) ~ EQUAL ~ operatorExpression ^^ {
                           case v~n~e~o => "single-variable-ex("+v+"="+o+")" }

// 11.4.2.2.3 Scoped constant assignments

  lazy val scopedConstantAssignmentExpression: Parser[String] =
             primaryExpression ~ not(WS) ~ DoubleCOLON ~ ConstantIdentifier ~ not(LineTerminator) ~
                 EQUAL ~ operatorExpression ^^ {
                           case p~n1~d~c~n2~e~o => "scoped-constant-ex("+p+"::"+c+"="+o+")" } |
             DoubleCOLON ~ ConstantIdentifier ~ not(LineTerminator) ~ EQUAL ~ operatorExpression ^^ {
                           case d~c~n2~e~o => "scoped-constant-ex(::"+c+"="+o+")" }
  lazy val scopedConstantAssignmentStatement: Parser[String] =
             primaryExpression ~ not(WS) ~ DoubleCOLON ~ ConstantIdentifier ~ not(LineTerminator) ~
                 EQUAL ~ methodInvocationWithoutParentheses ^^ {
                           case p~n1~d~c~n2~e~m => "scoped-constant-st("+p+"::"+c+"="+m+")" } |
             DoubleCOLON ~ ConstantIdentifier ~ not(LineTerminator) ~ EQUAL ~ methodInvocationWithoutParentheses ^^ {
                           case d~c~n2~e~m => "scoped-constant-st(::"+c+"="+m+")" }

// 11.4.2.2.4 Single indexing assignments

  lazy val singleIndexingAssignmentExpression: Parser[String] =
             primaryExpression  ~ not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ~ not(LineTerminator) ~
                 EQUAL ~ operatorExpression ^^ { case p~n1~l~i~r~n2~e~o => "single-indexing-ex("+p+"["+i+"]="+o+")" }
  lazy val singleIndexingAssignmentStatement: Parser[String] =
             primaryExpression  ~ not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ~ not(LineTerminator) ~
                 EQUAL ~ methodInvocationWithoutParentheses ^^ {
                           case p~n1~l~i~r~n2~e~m => "single-indexing-st("+p+"["+i+"]="+m+")" }

// 11.4.2.2.5 Single method assignments

  lazy val singleMethodAssignmentExpression: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ LocalVariableIdentifier ~
                 not(LineTerminator) ~ EQUAL ~ operatorExpression ^^ {
                           case p~n1~d~l~n2~e~o => "single-method-ex("+p+d+l+"="+o+")" } |
             primaryExpression ~ not(LineTerminator) ~ PERIOD ~ ConstantIdentifier ~ not(LineTerminator) ~
                 EQUAL ~ operatorExpression ^^ { case pe~n1~p~c~n2~e~o => "single-method-ex("+pe+p+c+"="+o+")" }
  lazy val singleMethodAssignmentStatement: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ LocalVariableIdentifier ~
                 not(LineTerminator) ~ EQUAL ~ methodInvocationWithoutParentheses ^^ {
                           case p~n1~d~l~n2~e~m => "single-method-st("+p+d+l+"="+m+")" } |
             primaryExpression ~ not(LineTerminator) ~ PERIOD ~ ConstantIdentifier ~ not(LineTerminator) ~
                 EQUAL ~ methodInvocationWithoutParentheses ^^ {
                           case pe~n1~p~c~n2~e~m => "single-method-st("+pe+p+c+"="+m+")" }

// 11.4.2.3 Abbreviated assignments

  lazy val abbreviatedAssignmentExpression: Parser[String] =
             abbreviatedVariableAssignmentExpression |
             abbreviatedIndexingAssignmentExpression |
             abbreviatedMethodAssignmentExpression
  lazy val abbreviatedAssignmentStatement: Parser[String] =
             abbreviatedVariableAssignmentStatement |
             abbreviatedIndexingAssignmentStatement |
             abbreviatedMethodAssignmentStatement

// 11.4.2.3.2 Abbreviated variable assignments

  lazy val abbreviatedVariableAssignmentExpression: Parser[String] =
             variable ~ not(LineTerminator) ~ assignmentOperator ~ operatorExpression ^^ {
                           case v~n~a~o => "abbreviated-variable-ex("+v+a+o+")" }
  lazy val abbreviatedVariableAssignmentStatement: Parser[String] =
             variable ~ not(LineTerminator) ~ assignmentOperator ~ methodInvocationWithoutParentheses ^^ {
                           case v~n~a~m => "abbreviated-variable-st("+v+a+m+")" }

// 11.4.2.3.3 Abbreviated indexing assignments

  lazy val abbreviatedIndexingAssignmentExpression: Parser[String] =
             primaryExpression ~ not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ~ not(LineTerminator) ~
                 assignmentOperator ~ operatorExpression ^^ {
                           case p~n1~l~i~r~n2~a~o => "abbreviated-indexing-ex("+p+l+i+r+a+o+")" }
  lazy val abbreviatedIndexingAssignmentStatement: Parser[String] =
             primaryExpression ~ not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ~ not(LineTerminator) ~
                 assignmentOperator ~ methodInvocationWithoutParentheses ^^ {
                           case p~n1~l~i~r~n2~a~m => "abbreviated-indexing-st("+p+l+i+r+a+m+")" }

// 11.4.2.3.4 Abbreviated method assignments

  lazy val abbreviatedMethodAssignmentExpression: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ LocalVariableIdentifier ~
                 not(LineTerminator) ~ assignmentOperator ~ operatorExpression ^^ {
                           case pe~n1~p~l~n2~a~o => "abbreviated-method-ex("+pe+p+l+a+o+")" } |
             primaryExpression ~ not(LineTerminator) ~ PERIOD ~ ConstantIdentifier ~ not(LineTerminator) ~
                 assignmentOperator ~ operatorExpression ^^ {
                           case pe~n1~p~c~n2~a~o => "abbreviated-method-st("+pe+p+c+a+o+")" }
  lazy val abbreviatedMethodAssignmentStatement: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ LocalVariableIdentifier ~
                 not(LineTerminator) ~  assignmentOperator ~ methodInvocationWithoutParentheses ^^ {
                           case pe~n1~p~l~n2~a~o => "abbreviated-method-st("+pe+p+l+a+o+")" } |
             primaryExpression ~ not(LineTerminator) ~ PERIOD ~ ConstantIdentifier ~ not(LineTerminator) ~
                 assignmentOperator ~ methodInvocationWithoutParentheses ^^ {
                           case pe~n1~p~c~n2~a~m => "abbreviated-method-st("+pe+p+c+a+m+")" }

  lazy val assignmentOperator: Parser[String] =
             AndASSIGN | OrASSIGN | HatASSIGN | AmpersandASSIGN | VerticalBarASSIGN | LtLtASSIGN | GtGtASSIGN |
             PlusASSIGN | MinusASSIGN | StarStarASSIGN | StarASSIGN | SlashASSIGN | PercentASSIGN

// 11.4.2.4 Multiple assignments

  lazy val multipleAssignmentStatement: Parser[String] =
             manyToOneAssignmentStatement |
             oneToPackingAssignmentStatement |
             manyToManyAssignmentStatement
  lazy val manyToOneAssignmentStatement: Parser[String] =
             leftHandSide ~ not(LineTerminator) ~ EQUAL ~ multipleRightHandSide ^^ {
                           case l~n~e~m => "many-to-one-st("+l+"="+m+")" }
  lazy val oneToPackingAssignmentStatement: Parser[String] =
             packingLeftHandSide ~ not(LineTerminator) ~
                 EQUAL ~ ( methodInvocationWithoutParentheses | operatorExpression ) ^^ {
                           case p~n~e~m => "one-to-packing-st("+p+"="+m+")" }
  lazy val manyToManyAssignmentStatement: Parser[String] =
             multipleLeftHandSide ~ not(LineTerminator) ~ EQUAL ~ multipleRightHandSide ^^ {
                           case ml~n~e~mr => "many-to-many-st("+ml+"="+mr+")" } |
             multipleButNotPackingLeftHandSide ~ not(LineTerminator) ~
                 EQUAL ~ ( methodInvocationWithoutParentheses | operatorExpression ) ^^ {
                           case ml~n~e~m => "many-to-many-st("+ml+"="+m+")" }
  lazy val multipleButNotPackingLeftHandSide: Parser[String] =
             not(packingLeftHandSide) ~ multipleLeftHandSide ^^ { case n~m => m }
  lazy val leftHandSide: Parser[String] =
             primaryExpression ~ not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ^^ {
                           case p~n~l~i~r => p+"["+i+"]" } |
             primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~
                 ( LocalVariableIdentifier | ConstantIdentifier ) ^^ { case pe~n~p~i => pe+p+i } |
             DoubleCOLON ~ ConstantIdentifier ^^ { case d~c => "::"+c } |
             variable
  lazy val multipleLeftHandSide: Parser[String] =
             ((( multipleLeftHandSideItem ~ not(LineTerminator) ~ COMMA ^^ {case m~n~c => m+", "})+) ^^ {
                   (_).mkString }) ~ ??(multipleLeftHandSideItem | packingLeftHandSide) ^^ { case s~m => s+m } |
             packingLeftHandSide |
             groupedLeftHandSide
  lazy val packingLeftHandSide: Parser[String] =
             STAR ~ ??(leftHandSide) ^^ { case s~l => s+l }
  lazy val groupedLeftHandSide: Parser[String] =
             LPAREN ~ multipleLeftHandSide ~ RPAREN ^^ { case l~m~r => "("+m+")" }
  lazy val multipleLeftHandSideItem: Parser[String] =
             leftHandSide |
             groupedLeftHandSide
  lazy val multipleRightHandSide: Parser[String] =
             operatorExpressionList ~ not(LineTerminator) ~ COMMA ~ splattingRightHandSide  ^^ {
                           case o~n~c~s => "multiple-right("+o+", "+s+")" } |
             operatorExpression ~ not(LineTerminator) ~ COMMA ~ operatorExpressionList ^^ {
                           case o1~n~c~o2 => "multiple-right("+o1+", "+o2+")" } |
             splattingRightHandSide ^^ { case s => "multiple-right("+s+")" }
  lazy val splattingRightHandSide: Parser[String] =
             splattingArgument ^^ { case s => "splatting-right("+s+")" }

// 11.4.2.5 Assignments with rescue modifiers

  lazy val assignmentWithRescueModifier: Parser[String] =
             leftHandSide ~ not(LineTerminator) ~ EQUAL ~ operatorExpression ~ not(LineTerminator) ~
                 RESCUE ~ operatorExpression ^^ { case l~n1~e~o1~n2~r~o2 => l+"="+o1+" rescue "+o2 }

// 11.4.3 Unary operator expressions

  lazy val unaryMinusExpression: Parser[String] =
             powerExpression  |
             MINUS ~ powerExpression ^^ { case m~p => "-"+p }
  lazy val unaryExpression: Parser[String] =
             primaryExpression |
             TILDE ~ unaryExpression ^^ { case t~u => "~"+u } |
             PLUS ~ unaryExpression ^^ { case p~u => "+"+u } |
             NotOP ~ unaryExpression ^^ { case n~u => "!"+u }

// 11.4.3.2 The defined? expression

  lazy val definedWithParentheses: Parser[String] =
             DEFINED ~ LPAREN ~ expression ~ RPAREN ^^ { case d~l~e~r => "defind?("+e+")" }
  lazy val definedWithoutParentheses: Parser[String] =
             DEFINED ~ operatorExpression ^^ { case d~o => "defind?("+o+")" }

// 11.4.4 Binary operator expressions

  lazy val equalityExpression: Parser[String] = memo(
             relationalExpression ~ ??(relationalOperation)  ^^ { case e~r => e+r } )
  lazy val relationalOperation: Parser[String] =
             not(LineTerminator) ~ relationalOperator ~ relationalExpression ^^ { case n~o~e => o+e }
  lazy val relationalOperator: Parser[String] =
             LtEqGT | DoubleEQ | TripleEQ | NotEQ | EqTILDE | NotTILDE
  lazy val relationalExpression: Parser[String] =
             chainl1 ( bitwiseOrExpression, not(LineTerminator) ~ (GT | GtEQ | LT | LtEQ) ^^ {
                           case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )
  lazy val bitwiseOrExpression: Parser[String] =
             chainl1 ( bitwiseAndExpression, not(LineTerminator) ~ (VerticalBAR | HAT) ^^ {
                           case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )
  lazy val bitwiseAndExpression: Parser[String] =
             chainl1 ( bitwiseShiftExpression, not(LineTerminator) ~ AMPERSAND ^^ {
                           case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )
  lazy val bitwiseShiftExpression: Parser[String] =
             chainl1 ( additiveExpression, not(LineTerminator) ~ (LtLT | GtGT) ^^ {
                           case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )
  lazy val additiveExpression: Parser[String] =
             chainl1 ( multiplicativeExpression, not(LineTerminator) ~ (PLUS | MINUS) ^^ {
                           case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )
  lazy val multiplicativeExpression: Parser[String] =
             chainl1 ( unaryMinusExpression, not(LineTerminator) ~ (MULT | DIV | PERCENT) ^^ {
                           case n~op => (l: String, r:String) => "(" + l+op+r +")" })
  lazy val powerExpression: Parser[String] =
             unaryExpression ~ not(LineTerminator) ~ StarSTAR ~ powerExpression ^^ {
                           case u~n~s~p => "("+u+"**"+p+")" } |
             unaryExpression

// 11.5 Primary expressions

  lazy val primaryExpression: Parser[String] = memo(
             r1chain ( primaryExpression1,
                 not(WS) ~ DoubleCOLON ~ ConstID ^^ { case n~d~c => "::" + c } | //scoped-constant-reference
                 not(LineTerminator) ~ PERIOD ~ methodName ~ not(argumentWithoutParentheses) ~
                     ??(argumentWithParentheses) ~ ??(block) ^^ {
                            case n1~p~m~n2~a~b => "." + m + a + b } | //primary-method-invocation
                 not(LineTerminator) ~ DoubleCOLON ~ methodName ~ argumentWithParentheses ~ ??(block) ^^ {
                            case n~d~m~a~b => "::" + m + a + b } | //primary-method-invocation
                 not(LineTerminator) ~ DoubleCOLON ~ methodNameExceptConstant ~ ??(block) ^^ {
                            case n~d~m~b => "::" + m + b } | //primary-method-invocation
                 not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ~ not(EQUAL) ^^ {
                            case n1~l~i~r~n2 => "["+i+"]" } //indexing-method-invocation
              ) { case (x, y) => "(" + x + y + ")" } )

  lazy val primaryExpression1: Parser[String] =
             classDefinition |
             singletonClassDefinition |
             moduleDefinition |
             methodDefinition |
             singletonMethodDefinition |
             yieldWithOptionalArgument |
             unlessExpression |
             caseExpression |
             whileExpression |
             untilExpression |
             forExpression |
             returnWithoutArgument |
             breakWithoutArgument |
             nextWithoutArgument |
             redoExpression |
             retryExpression |
             beginExpression |
             arrayConstructor |
             hashConstructor |
             definedWithParentheses |
             groupingExpression |
             scopedConstantReference |
             primaryMethodInvocation ^^ { case p => "primary-method-inv("+p+")" }|
             literal |
             variableReference

// 11.5.2.2.2 The if expression

  lazy val ifExpression: Parser[String] =
             IF ~ expression ~ thenClause ~ (((elsifClause)*) ^^ { (_).mkString("", "\n", "") }) ~
                 ??(elseClause) ~ END ^^ { case i~ex~t~ei~el~en => "if "+ex+" "+t+" "+ei+"\n "+el+" end" }
  lazy val thenClause: Parser[String] =
             separator ~ compoundStatement ^^ { case s~c => s+c } |
             separator ~ THEN ~ compoundStatement ^^ { case s~t~c => s+" then "+c }
  lazy val elseClause: Parser[String] =
             ELSE ~ compoundStatement ^^ { case e~c => "else "+c }
  lazy val elsifClause: Parser[String] =
             ELSIF ~ expression ~ thenClause ^^ { case ei~e~t => "elsif "+e+t }

// 11.5.2.2.3 The unless expression

  lazy val unlessExpression: Parser[String] =
             UNLESS ~ expression ~ thenClause ~ ??(elseClause) ~ END ^^ {
                            case u~e~t~el~en => "unless "+e+t+el+" end" }

// 11.5.2.2.4 The case expression

  lazy val caseExpression: Parser[String] =
             caseExpressionWithExpression |
             caseExpressionWithoutExpression
  lazy val caseExpressionWithExpression: Parser[String] =
             CASE ~ expression ~ ??(separatorList) ~ (((whenClause)+) ^^ { (_).mkString("", "\n", "") }) ~
                 ??(elseClause) ~ END ^^ { case c~ex~s~w~el~en => "case "+ex+s+w+el+" end" }
  lazy val caseExpressionWithoutExpression: Parser[String] =
             CASE ~ ??(separatorList) ~ (((whenClause)+) ^^ { (_).mkString("", "\n", "") }) ~
                 ??(elseClause) ~ END ^^ { case c~s~w~el~en => "case "+s+w+el+" end" }
  lazy val whenClause: Parser[String] = WHEN ~ whenArgument ~ thenClause ^^ {
                             case w~a~t => "when "+a+t }
  lazy val whenArgument: Parser[String] =
             operatorExpressionList ~ ??(not(LineTerminator) ~ COMMA ~ splattingArgument ^^ {
                             case n~c~s => ", "+s }) ^^ { case o~s => o+s } |
             splattingArgument

// 11.5.2.2.5 Conditional operator expression

  lazy val conditionalOperatorExpression: Parser[String] =
             rangeExpression ~ not(LineTerminator) ~ QUESTION ~ operatorExpression ~
                 not(LineTerminator) ~ COLON ~ operatorExpression  ^^ {
                             case r~n1~q~o1~n2~c~o2 => "(" + r + "?" + o1 + ":" + o2 + ")" } |
             rangeExpression

// 11.5.2.3.2 The while expression

  lazy val whileExpression: Parser[String] =
             WHILE ~ expression ~ doClause ~ END ^^ { case w~ex~d~en => "while "+ex+d+" end-while" }
  lazy val doClause: Parser[String] =
             separator ~ compoundStatement ^^ { case s~c => s+c } |
             not(LineTerminator) ~ DO ~ compoundStatement ^^ { case n~d~c => "do "+c }

// 11.5.2.3.3 The until expression

  lazy val untilExpression: Parser[String] = UNTIL ~ expression ~ doClause ~ END ^^ {
             case u~ex~d~en => "until "+ex+d+" end-until" }

// 11.5.2.3.4 The for expression
  lazy val forExpression: Parser[String] =
             FOR ~ forVariable ~ not(LineTerminator) ~ IN ~ expression ~ doClause ~ END ^^ {
                            case f~v~n~i~ex~d~en => "for "+v+" in "+ex+d+" end-for" }
  lazy val forVariable: Parser[String] =
             leftHandSide |
             multipleLeftHandSide

// 11.5.2.4.2 The return expression

  lazy val returnWithoutArgument: Parser[String] =
             RETURN
  lazy val returnWithArgument: Parser[String] =
             RETURN ~ jumpArgument ^^ {case r~j => "(return "+j+")" }
  lazy val jumpArgument: Parser[String] =
             not(LineTerminator) ~ argumentList ^^ { case n~a => a }

// 11.5.2.4.3 The break expression

  lazy val breakWithoutArgument: Parser[String] =
             BREAK
  lazy val breakWithArgument: Parser[String] =
             BREAK ~ jumpArgument ^^ {case b~j => "(break "+j+")" }

// 11.5.2.4.4 The next expression

  lazy val nextWithoutArgument: Parser[String] =
             NEXT
  lazy val nextWithArgument: Parser[String] =
             NEXT ~ jumpArgument ^^ {case n~j => "(next "+j+")" }

// 11.5.2.4.5 The redo expression

  lazy val redoExpression: Parser[String] =
             REDO

// 11.5.2.4.6 The retry expression

  lazy val retryExpression: Parser[String] =
             RETRY

// 11.5.2.5 The begin expression

  lazy val beginExpression: Parser[String] =
             BEGIN ~ bodyStatement ~ END ^^ { case b~s~e => "begin "+s+" end-begin" }
  lazy val bodyStatement: Parser[String] =
             compoundStatement ~ (((rescueClause)*) ^^ { (_).mkString("", "\n", "") }) ~ ??(elseClause) ~
                 ??(ensureClause) ^^ { case c~r~el~en => c+r+el+en }
  lazy val rescueClause: Parser[String] =
             RESCUE ~ not(LineTerminator) ~ ??(exceptionClassList) ~ ??(exceptionVariableAssignment) ~
                 thenClause ^^ { case r~n~ec~ea~t => "rescue "+ec+ea+t }
  lazy val exceptionClassList: Parser[String] =
             operatorExpression |
             multipleRightHandSide
  lazy val exceptionVariableAssignment: Parser[String] =
             EqGT ~ leftHandSide ^^ { case e~l => "=>"+l }
  lazy val ensureClause: Parser[String] =
             ENSURE ~ compoundStatement ^^ { case e~c => "ensure "+c }

// 11.5.3 Grouping expression

  lazy val groupingExpression: Parser[String] =
             LPAREN~>compoundStatement<~RPAREN

// 11.5.4 Variable references

  lazy val variableReference: Parser[String] =
             variable |
             pseudoVariable
  lazy val variable: Parser[String] =
             ConstantIdentifier |
             GlobalVariableIdentifier |
             ClassVariableIdentifier |
             InstanceVariableIdentifier |
             LocalVariableIdentifier
  lazy val scopedConstantReference: Parser[String] =
             DoubleCOLON ~ ConstID ^^ { case d~c => "::"+c } // |
             // primaryExpression ~ not(WS) ~ DoubleCOLON ~ ConstID

// 11.5.4.8 Pseudo variables

  lazy val pseudoVariable: Parser[String] =
             nilExpression |
             trueExpression |
             falseExpression |
             selfExpression

// 11.5.4.8.2 The nil expression

  lazy val nilExpression: Parser[String] =
             NIL

// 11.5.4.8.3 The true expression and the false expression

  lazy val trueExpression: Parser[String] =
             TRUE
  lazy val falseExpression: Parser[String] =
             FALSE

// 11.5.4.8.4 The self expression

  lazy val selfExpression: Parser[String] =
             SELF

// 11.5.5.1 Array constructor

  lazy val arrayConstructor: Parser[String] =
             LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ^^ { case l~i~r => "["+i+"]" }

// 11.5.5.2 Hash constructor

  lazy val hashConstructor: Parser[String] =
             LBRACE ~ ??(associationList ~ not(LineTerminator) ~ ??(COMMA) ^^ { case a~n~c => a+c } ) ~
                 RBRACE ^^ { case l~a~r => "{"+a+"}" }
  lazy val associationList: Parser[String] =
             rep1sep ( association, not(LineTerminator) ~ COMMA ) ^^ { (_).mkString("", ", ", "") }
  lazy val association: Parser[String] =
             associationKey ~ not(LineTerminator) ~ EqGT ~ associationValue ^^ {
                                 case k~n~e~v => k+"=>"+v }
  lazy val associationKey: Parser[String] =
             operatorExpression
  lazy val associationValue: Parser[String] =
             operatorExpression

// 11.5.5.3 Range expression

  lazy val rangeExpression: Parser[String] =
             operatorOrExpression ~ not(LineTerminator) ~ rangeOperator ~ operatorOrExpression ^^ {
                                 case o1~n~r~o2 => o1 + r + o2 } |
             operatorOrExpression
  lazy val rangeOperator: Parser[String] =
             DoublePERIOD |
             TriplePERIOD

// 12 Statements

  lazy val statement: Parser[String] = memo(
             r1chain ( statement1,
                 not(LineTerminator) ~ IF ~ expression ^^ { case n~i~e => " if "+e } | // 12.3 The if modifier statement
                 not(LineTerminator) ~ UNLESS ~ expression ^^ { case n~u~e => " unless "+e } | // 12.4 unless modifier
                 not(LineTerminator) ~ WHILE ~ expression ^^ { case n~w~e => " while "+e } | // 12.5 while modifier
                 not(LineTerminator) ~ UNTIL ~ expression ^^ { case n~u~e => " until "+e } | // 12.6 until modifier
                 not(LineTerminator) ~ RESCUE ~ fallbackStatement ^^ { case n~r~f => " rescue "+f } // 12.7 rescue modifier
              ) { case (x, y) => "(" + x + y + ")" } )
  lazy val fallbackStatement: Parser[String] =
             statement1 // not(keywordAndExpression) ~ not(keywordOrExpression)
  lazy val statement1: Parser[String] =
             assignmentStatement |
             expressionStatement |
             aliasStatement |
             undefStatement

// 12.2 Expression statement

  lazy val expressionStatement: Parser[String] =
             expression

// 13.1.2 Module definition

  lazy val moduleDefinition: Parser[String] =
             MODULE ~ modulePath ~ moduleBody ~ END ^^ { case m~p~b~e => "module "+p+b+"\nend-module" }
  lazy val modulePath: Parser[String] =
             topModulePath |
             moduleName |
             nestedModulePath
  lazy val moduleName: Parser[String] =
             ConstantIdentifier
  lazy val topModulePath: Parser[String] =
             DoubleCOLON ~ moduleName ^^ { case d~m => "::"+m }
  lazy val nestedModulePath: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ DoubleCOLON ~ moduleName ^^ { case p~n~d~m => p+"::"+m }
  lazy val moduleBody: Parser[String] =
             bodyStatement

// 13.2.2 Class definition

  lazy val classDefinition: Parser[String] =
             CLASS ~ classPath ~ ??( not(LineTerminator) ~ LT ~ superclass ^^ { case n~l~s => "< "+s } ) ~
                 separator ~ classBody ~  END ^^ { case c~p~su~sep~b~e => "class "+p+su+sep+b+"\nend-class" }
  lazy val classPath: Parser[String] =
             topClassPath |
             className |
             nestedClassPath
  lazy val className: Parser[String] =
             ConstantIdentifier
  lazy val topClassPath: Parser[String] =
             DoubleCOLON ~ className ^^ { case d~c => "::"+c }
  lazy val nestedClassPath: Parser[String] =
             primaryExpression ~ not(LineTerminator) ~ DoubleCOLON ~ className ^^ { case p~n~d~m => p+"::"+m }
  lazy val superclass: Parser[String] =
             expression
  lazy val classBody: Parser[String] =
             bodyStatement

// 13.3.1 Method definition

  lazy val methodDefinition: Parser[String] =
             DEF ~ definedMethodName ~ ( not(LineTerminator) ~ methodParameterPart ^^ { case n~m => m } |
                separator ^^ { case s => "; " } ) ~ methodBody ~ END ^^ {
                               case d~na~p~b~e => "def "+na+p+b+" end-def" }
  lazy val definedMethodName: Parser[String] =
             methodName |
             AssignmentLikeMethodIdentifier
  lazy val methodBody: Parser[String] =
             bodyStatement

// 13.3.2 Method parameters

  lazy val methodParameterPart: Parser[String] =
             LPAREN ~ ??(parameterList) ~ RPAREN ^^ { case l~p~r => "("+p+")" } |
             parameterList ~ separator ^^ { case p~s => p+"; " }
  lazy val parameterList: Parser[String] =
             mandatoryParameterList ~ ??( COMMA ~ optionalParameterList ^^ { case c~p => ", "+p } ) ~
                 ??( COMMA ~ arrayParameter ^^ { case c~p => ", "+p } ) ~ ??( COMMA ~ procParameter ^^ {
                              case c~p => ", "+p } ) ^^ { case m~o~a~p => m+o+a+p } |
             optionalParameterList ~ ??( COMMA ~ arrayParameter ^^ { case c~p => ", "+p } ) ~
                 ??( COMMA ~ procParameter ^^ { case c~p => ", "+p } ) ^^ { case o~a~p => o+a+p } |
             arrayParameter ~ ??( COMMA ~ procParameter ^^ { case c~p => ", "+p } ) ^^ { case a~p => a+p } |
             procParameter
  lazy val mandatoryParameterList: Parser[String] =
             rep1sep(mandatoryParameter, COMMA) ^^ { (_).mkString("", ", ", "") }
  lazy val mandatoryParameter: Parser[String] =
             LocalVariableIdentifier
  lazy val optionalParameterList: Parser[String] =
             rep1sep(optionalParameter, COMMA) ^^ { (_).mkString("", ", ", "") }
  lazy val optionalParameter: Parser[String] =
             optionalParameterName ~ EQUAL ~ defaultParameterExpression ^^ { case o~e~d => o+"="+d }
  lazy val optionalParameterName: Parser[String] =
             LocalVariableIdentifier
  lazy val defaultParameterExpression: Parser[String] =
             operatorExpression
  lazy val arrayParameter: Parser[String] =
             STAR ~ arrayParameterName ^^ { case s~a => "*"+a } |
             STAR
  lazy val arrayParameterName: Parser[String] =
             LocalVariableIdentifier
  lazy val procParameter: Parser[String] =
             AMPERSAND ~ procParameterName ^^ { case a~p => "&"+p }
  lazy val procParameterName: Parser[String] =
             LocalVariableIdentifier

// 13.3.6 The alias statement

  lazy val aliasStatement: Parser[String] =
             ALIAS ~ newName ~ aliasedName ^^ { case a~nn~an => "alias("+nn+", "+an+")" }
  lazy val newName: Parser[String] =
             methodNameOrSymbol
  lazy val aliasedName: Parser[String] =
             methodNameOrSymbol
  lazy val methodNameOrSymbol: Parser[String] =
             definedMethodName |
             Symbol

// 13.3.7 The undef statement

  lazy val undefStatement: Parser[String] =
             UNDEF ~ undefList ^^ { case u~l => "undef("+l+")" }
  lazy val undefList: Parser[String] =
             rep1sep(methodNameOrSymbol, COMMA) ^^ { (_).mkString("", ", ", "") }

// 13.4.2 Singleton class definition

  lazy val singletonClassDefinition: Parser[String] =
             CLASS ~ LtLT ~ expression ~ separator ~ singletonClassBody ~ END ^^ {
                                 case c~l~ex~s~b~en => "class << "+ex+s+b+"\nend-s-class" }
  lazy val singletonClassBody: Parser[String] =
             bodyStatement

// 13.4.3 Singleton method definition

  lazy val singletonMethodDefinition: Parser[String] =
             DEF ~ singletonObject ~ (PERIOD | DoubleCOLON) ~ definedMethodName ~
                 ( not(LineTerminator) ~ methodParameterPart ^^ { case n~m => m } |
                   separator ^^ { case s => ";" } ) ~ methodBody ~ END ^^ {
                                 case d~s~pc~dn~p~b~e => "def "+s+pc+dn+p+b+" end-def" }
  lazy val singletonObject: Parser[String] =
             variableReference |
             LPAREN ~ expression ~ RPAREN ^^ { case l~e~r => "("+e+")" }

// 8.5 Comments

  lazy val Comment: Parser[String] =
             SingleLineComment |
             MultiLineComment
  lazy val SingleLineComment: Parser[String] =
             regex("""#""".r) ~ CommentContent ^^ { case s~c => s+c }
  lazy val CommentContent: Parser[String] =
             LineContent
  lazy val LineContent: Parser[String] =
             (( not(LineTerminator) ~ SourceCharacter ^^ { case n~s => s } )*) ^^ { (_).mkString }
  lazy val MultiLineComment: Parser[String] =
             MultiLineCommentBeginLine ~ ((MultiLineCommentLine*) ^^ { (_).mkString } ) ~
                 MultiLineCommentEndLine ^^ { case b~c~e => b+c+e }
  lazy val MultiLineCommentBeginLine: Parser[String] =
             regex("""^=begin""".r) ~ ??(RestOfBeginEndLine) ~ LineTerminator ^^ { case b~r~l => b+r+l }
  lazy val MultiLineCommentEndLine: Parser[String] =
             regex("""^=end""".r) ~ ??(RestOfBeginEndLine) ~ ( LineTerminator | regex("""\z""".r) ) ^^ {
                                       case e~r~l => e+r+"\n" }
  lazy val RestOfBeginEndLine: Parser[String] =
               (((not(LineTerminatorEscapeSequence) ~ Whitespace ^^ { case n~w => w })+) ^^ {
                                       (_).mkString } ) ~ CommentContent ^^ { case w~c => w+c }
  lazy val MultiLineCommentLine: Parser[String] =
             not(MultiLineCommentEndLine) ~ CommentLine ^^ { case n~c => c }
  lazy val CommentLine: Parser[String] =
             CommentContent ~ LineTerminator ^^ { case c~l => c+"\n" }

// 8.7.2 Keywords

  lazy val Keyword: Parser[String] =
             KeywordString ~ not(IdentifierCharacter) ^^ { case k~n => k }
  lazy val KeywordString: Parser[String] =
             LINE | ENCODING | FILE | UpBEGIN | UpEND | ALIAS | AND | BEGIN | BREAK |
             CASE | CLASS | DEF | DEFINED | DO | ELSE | ELSIF | END | ENSURE | FOR | FALSE | IF | IN |
             MODULE | NEXT | NIL | NOT | OR | REDO | RESCUE | RETRY | RETURN | SELF | SUPER | THEN |
             TRUE | UNDEF | UNLESS | UNTIL | WHEN | WHILE | YIELD

// 8.7.3 Identifiers

  lazy val LocalVariableIdentifier: Parser[String] =
             not(Keyword) ~ not(MethodID) ~ LocalID ^^ { case n1~n2~l => l }
  lazy val GlobalVariableIdentifier: Parser[String] =
             GlobalID
  lazy val ClassVariableIdentifier: Parser[String] =
             ClassID
  lazy val InstanceVariableIdentifier: Parser[String] =
             InstanceID
  lazy val ConstantIdentifier: Parser[String] =
             ConstID
  lazy val MethodOnlyIdentifier: Parser[String] =
             (LocalID | ConstID) ~ (NotOP | QUESTION) ^^ { case i~q => i+q }
  lazy val AssignmentLikeMethodIdentifier: Parser[String] =
             ( ConstantIdentifier | LocalVariableIdentifier ) ~ EQUAL ^^ { case i~e => i+"=" }


  lazy val OperatorMethodName: Parser[String] =
             HAT | AMPERSAND | VerticalBAR | EQUAL | DoubleEQ | TripleEQ | EqTILDE | GT | GtEQ |
             LT | LtEQ | LtLT | GtGT | PLUS | MINUS | STAR | SLASH | PERCENT | StarSTAR |
            TILDE | PlusAT | MinusAT | ArrayEQ | ArraySYM | BackQUOTE

// 8.7.6 Literals

  lazy val literal: Parser[String] =
             anySpace~>literalString
  lazy val literalString: Parser[String] =
             NumericLiteral |
             StringLiteral |
             ArrayLiteral |
             RegularExpressionLiteral |
             Symbol

// 8.7.6.2 Numeric literals

  lazy val NumericLiteral: Parser[String] =
             SignedNumber |
             UnsignedNumber
  lazy val SignedNumber: Parser[String] =
             regex("""\+|\-""".r) ~ UnsignedNumber ^^ { case r~u => r+u }
  lazy val UnsignedNumber: Parser[String] =
             FloatLiteral |
             IntegerLiteral
  lazy val IntegerLiteral: Parser[String] =
             DecimalIntegerLiteral |
             BinaryIntegerLiteral |
             OctalIntegerLiteral |
             HexadecimalIntegerLiteral
  lazy val DecimalIntegerLiteral: Parser[String] =
             UnprefixedDecimalIntegerLiteral |
             PrefixedDecimalIntegerLiteral
  lazy val UnprefixedDecimalIntegerLiteral: Parser[String] =
             regex("""0""".r) |
             DecimalDigitExceptZero ~ ((( ??(regex("""_""".r)) ~ DecimalDigit ^^ { case r~d => r+d } )*) ^^ {
                                       (_).mkString } ) ^^ { case d~r => d+r }
  lazy val PrefixedDecimalIntegerLiteral: Parser[String] =
             regex("""0(d|D)""".r) ~ DigitDecimalPart ^^ { case r~d => r+d }
  lazy val DigitDecimalPart: Parser[String] =
             DecimalDigit ~ ((( ??(regex("""_""".r)) ~ DecimalDigit ^^ { case r~d => r+d } )*) ^^ {
                                (_).mkString } ) ^^ { case d~r => d+r }
  lazy val BinaryIntegerLiteral: Parser[String] =
             regex("""0(b|B)""".r) ~ BinaryDigit ~ ((( ??(regex("""_""".r)) ~ BinaryDigit ^^ {
                                  case r~b => r+b } )*) ^^ { (_).mkString } ) ^^ { case r~b1~b2 => r+b1+b2 }
  lazy val OctalIntegerLiteral: Parser[String] =
             regex("""0(_|o|O)""".r) ~ OctalDigit ~ ((( ??(regex("""_""".r)) ~ OctalDigit ^^ {
                                  case r~o => r+o } )*) ^^ { (_).mkString } ) ^^ { case r~o1~o2 => r+o1+o2 }
  lazy val HexadecimalIntegerLiteral: Parser[String] =
             regex("""0(x|X)""".r) ~ HexadecimalDigit ~ ((( ??(regex("""_""".r)) ~ HexadecimalDigit ^^ {
                                  case r~x => r+x } )*) ^^ { (_).mkString } ) ^^ { case r~x1~x2 => r+x1+x2 }
  lazy val FloatLiteral: Parser[String] =
             FloatLiteralWithExponent |
             FloatLiteralWithoutExponent
  lazy val FloatLiteralWithoutExponent: Parser[String] =
             UnprefixedDecimalIntegerLiteral ~ regex("""\.""".r) ~ DigitDecimalPart ^^ { case u~r~d => u+"."+d }
  lazy val FloatLiteralWithExponent: Parser[String] =
             SignificandPart ~ ExponentPart ^^ { case s~e => s+e }
  lazy val SignificandPart: Parser[String] =
             FloatLiteralWithoutExponent |
             UnprefixedDecimalIntegerLiteral
  lazy val ExponentPart: Parser[String] =
             regex("""(e|E)(\+|\-)?""".r) ~ DigitDecimalPart ^^ { case e~d => e+d }
  lazy val DecimalDigit: Parser[String] =
             regex("""[0-9]""".r)
  lazy val DecimalDigitExceptZero: Parser[String] =
             regex("""[1-9]""".r)
  lazy val BinaryDigit: Parser[String] =
             regex("""0|1""".r)
  lazy val OctalDigit: Parser[String] =
             regex("""[0-7]""".r)
  lazy val HexadecimalDigit: Parser[String] =
             DecimalDigit |
             regex("""[a-fA-F]""".r)

// 8.7.6.3 String literals

  lazy val StringLiteral: Parser[String] =
             SingleQuotedString |
             DoubleQuotedString |
             QuotedNonExpandedLiteralString |
             QuotedExpandedLiteralString |
             HereDocument |
             ExternalCommandExecution

// 8.7.6.3.2 Single quoted strings

  lazy val SingleQuotedString: Parser[String] =
             regex("""'""".r) ~ ((SingleQuotedStringCharacter*) ^^ { (_).mkString } ) ~ regex("""'""".r) ^^ {
                                   case r1~s~r2 => "'"+s+"'" }
  lazy val SingleQuotedStringCharacter: Parser[String] =
             SingleQuotedStringNonEscapedCharacter |
             SingleQuotedEscapeSequence
  lazy val SingleQuotedEscapeSequence: Parser[String] =
             SingleEscapeCharacterSequence |
             SingleQuotedStringNonEscapedCharacterSequence
  lazy val SingleEscapeCharacterSequence: Parser[String] =
             regex("""\\""".r) ~ SingleQuotedStringMetaCharacter ^^ { case r~s => "\\"+s }
  lazy val SingleQuotedStringNonEscapedCharacterSequence: Parser[String] =
             regex("""\\""".r) ~ SingleQuotedStringNonEscapedCharacter ^^ { case r~s => "\\"+s }
  lazy val SingleQuotedStringMetaCharacter: Parser[String] =
             regex("""'|\\""".r)
  lazy val SingleQuotedStringNonEscapedCharacter: Parser[String] =
             not(SingleQuotedStringMetaCharacter) ~ SourceCharacter ^^ { case n~s => s }

// 8.7.6.3.3 Double quoted strings

  lazy val DoubleQuotedString: Parser[String] =
             regex(""""""".r) ~ ((DoubleQuotedStringCharacter*) ^^ { (_).mkString } ) ~ regex(""""""".r) ^^ {
                                   case r1~s~r2 => "\""+s+"\"" }
  lazy val DoubleQuotedStringCharacter: Parser[String] =
             not(regex(""""|#|\\""".r)) ~ SourceCharacter ^^ { case n~s => s } |
             regex("""#""".r) ~ not(LookaheadCharacter) ^^ { case r~n => "#" } |
             DoubleEscapeSequence |
             InterpolatedCharacterSequence
  lazy val LookaheadCharacter: Parser[String] =
             regex("""\$|@|\{""".r)
  lazy val DoubleEscapeSequence: Parser[String] =
             SimpleEscapeSequence |
             NonEscapedSequence |
             LineTerminatorEscapeSequence |
             OctalEscapeSequence |
             HexadecimalEscapeSequence |
             ControlEscapeSequence
  lazy val SimpleEscapeSequence: Parser[String] =
             regex("""\\""".r) ~ DoubleEscapedCharacter ^^ { case r~d => "\\"+d }
  lazy val DoubleEscapedCharacter: Parser[String] =
             regex("""n|t|r|f|v|a|e|b|s""".r)
  lazy val NonEscapedSequence: Parser[String] =
             regex("""\\""".r) ~ NonEscapedDoubleQuotedStringCharacter ^^ { case r~n => "\\"+n }
  lazy val NonEscapedDoubleQuotedStringCharacter: Parser[String] =
             not( AlphaNumericCharacter | LineTerminator ) ~ SourceCharacter ^^ { case n~s => s }
  lazy val OctalEscapeSequence: Parser[String] =
             regex("""\\""".r) ~ OctalDigit ~ ??(OctalDigit) ~ ??(OctalDigit) ^^ {
                                     case r~o1~o2~o3 => "\\"+o1+o2+o3 }
  lazy val HexadecimalEscapeSequence: Parser[String] =
             regex("""\\x""".r) ~ HexadecimalDigit ~ ??(HexadecimalDigit) ^^ { case r~h1~h2 => "\\x"+h1+h2 }
  lazy val ControlEscapeSequence: Parser[String] =
             regex("""\\(C\-|c)""".r) ~ ControlEscapeCharacter ^^ { case r~c => r+c }
  lazy val ControlEscapeCharacter: Parser[String] =
             DoubleEscapeSequence |
             regex("""\?""".r) |
             not( regex("""\\|\?""".r) ) ~ SourceCharacter ^^ { case n~s => s }
  lazy val InterpolatedCharacterSequence: Parser[String] =
             regex("""#""".r) ~ GlobalVariableIdentifier ^^ { case r~i => "#"+i } |
             regex("""#""".r) ~ ClassVariableIdentifier ^^ { case r~i => "#"+i } |
             regex("""#""".r) ~ InstanceVariableIdentifier ^^ { case r~i => "#"+i } |
             regex("""#\{""".r) ~ compoundStatement ~ regex("""\}""".r) ^^ { case r1~s~r2 => "#{"+s+"}" }
  lazy val AlphaNumericCharacter: Parser[String] =
             UppercaseCharacter |
             LowercaseCharacter |
             DecimalDigit

// 8.7.6.3.4 Quoted non-expanded literal strings

  lazy val QuotedNonExpandedLiteralString: Parser[String] =
             regex("""%q""".r) ~ NonExpandedDelimitedString ^^ { case r~n => "%q"+n }
  lazy val NonExpandedDelimitedString: Parser[String] =
             LiteralBeginningDelimiter >> {
                 case literalBegin => ( (NonExpandedLiteralString(literalBegin)*) ^^ { (_).mkString } ) ~
                          LiteralEndingDelimiter2(literalBegin) ^^ { case s~e => literalBegin+s+e } }
  def NonExpandedLiteralString(literalBegin: String): Parser[String] =
             (if (literalBegin == "{" || literalBegin == "(" || literalBegin == "[" || literalBegin == "<" )
                 NonExpandedDelimitedString2(literalBegin) else failure("not matched") ) |
             NonExpandedLiteralCharacter(literalBegin)
  def NonExpandedDelimitedString2(literalBegin: String): Parser[String] =
             LiteralBeginningDelimiter2(literalBegin) ~ ((NonExpandedLiteralString(literalBegin)*) ^^ {
                  (_).mkString } ) ~ LiteralEndingDelimiter2(literalBegin) ^^ {  case b~s~e => b+s+e }
  def NonExpandedLiteralCharacter(literalBegin: String): Parser[String] =
             NonEscapedLiteralCharacter(literalBegin) |
             NonExpandedLiteralEscapeSequence(literalBegin)
  def NonEscapedLiteralCharacter(literalBegin: String): Parser[String] =
             not(QuotedLiteralEscapeCharacter(literalBegin)) ~ SourceCharacter ^^ { case n~s => s }
  def NonExpandedLiteralEscapeSequence(literalBegin: String): Parser[String] =
             NonExpandedLiteralEscapeCharacterSequence(literalBegin) |
             NonEscapedNonExpandedLiteralCharacterSequence(literalBegin)
  def NonExpandedLiteralEscapeCharacterSequence(literalBegin: String): Parser[String] =
             regex("""\\""".r) ~ NonExpandedLiteralEscapedCharacter(literalBegin) ^^ { case r~n => "\\"+n }
  def NonExpandedLiteralEscapedCharacter(literalBegin: String): Parser[String] =
             LiteralBeginningDelimiter2(literalBegin) | LiteralEndingDelimiter2(literalBegin) | regex("""\\""".r)
  def QuotedLiteralEscapeCharacter(literalBegin: String): Parser[String] =
             NonExpandedLiteralEscapedCharacter(literalBegin)
  def NonEscapedNonExpandedLiteralCharacterSequence(literalBegin: String): Parser[String] =
             regex("""\\""".r) ~ NonEscapedNonExpandedLiteralCharacter(literalBegin) ^^ { case r~n => "\\"+n }
  def NonEscapedNonExpandedLiteralCharacter(literalBegin: String): Parser[String] =
             not(NonExpandedLiteralEscapedCharacter(literalBegin)) ~ SourceCharacter ^^ { case n~s => s }
  lazy val LiteralBeginningDelimiter: Parser[String] =
             not(AlphaNumericCharacter) ~ SourceCharacter ^^ { case n~s => s }
  lazy val LiteralEndingDelimiter: Parser[String] =
             not(AlphaNumericCharacter) ~ SourceCharacter ^^ { case n~s => s }
  def LiteralBeginningDelimiter2(beginString: String): Parser[String] =
             LiteralBeginningDelimiter ^? { case b if beginString == b => b }
  def LiteralEndingDelimiter2(beginString: String): Parser[String] =
             LiteralEndingDelimiter ^? {
                 case "}" if beginString == "{" => "}"
                 case ")" if beginString == "(" => ")"
                 case "]" if beginString == "[" => "]"
                 case ">" if beginString == "<" => ">"
                 case e if (beginString != "{" && beginString != "(" && beginString != "[" && beginString != "<") &&
                             beginString == e => e }

// 8.7.6.3.5 Quoted expanded literal strings

  lazy val QuotedExpandedLiteralString: Parser[String] =
             regex("""%Q?""".r) ~ ExpandedDelimitedString ^^ { case r~n => r+n }
  lazy val ExpandedDelimitedString: Parser[String] =
             LiteralBeginningDelimiter >> { case literalBegin => ((ExpandedLiteralString(literalBegin)*) ^^ {
                 (_).mkString } ) ~ LiteralEndingDelimiter2(literalBegin) ^^ { case s~e => literalBegin+s+e } }
  def ExpandedLiteralString(literalBegin: String): Parser[String] =
             (if (literalBegin == "{" || literalBegin == "(" || literalBegin == "[" || literalBegin == "<" )
                 ExpandedDelimitedString2(literalBegin) else failure("not matched") ) |
             ExpandedLiteralCharacter(literalBegin)
  def ExpandedDelimitedString2(literalBegin: String): Parser[String] =
             LiteralBeginningDelimiter2(literalBegin) ~ ((ExpandedLiteralString(literalBegin)*) ^^ { (_).mkString } ) ~
                 LiteralEndingDelimiter2(literalBegin) ^^ {  case b~s~e => b+s+e }
  def ExpandedLiteralCharacter(literalBegin: String): Parser[String] =
             not(regex("""#""".r)) ~ NonEscapedLiteralCharacter(literalBegin) ^^ { case n~c => c } |
             not(regex("""\$|@|\{""".r)) ~ regex("""#""".r) ^^ { case n~r => r } |
             DoubleEscapeSequence |
             InterpolatedCharacterSequence

// 8.7.6.3.6 Here documents

  def rebuild(a: Reader[Char], newSource: String, newOffset: Int): Reader[Char] = new Reader[Char] {
    def atEnd = a.atEnd
    def first = a.first
    def pos = a.pos
    def rest = rebuild(a.rest, newSource, offset + 1)
    override def source = newSource
    override def offset = newOffset
  }
  def concat(a: Reader[Char], b: Reader[Char]): Reader[Char] = new PackratReader({
    val aSource = a.source + b.source.subSequence(b.offset, b.source.length()).toString
    if(a.atEnd) {
      rebuild(b, aSource, a.offset)
    } else {
      new Reader[Char] {
        private lazy val result = concat(a.rest, b)
        def atEnd = a.atEnd
        def first = a.first
        def pos = a.pos
        def rest = result
        override def source = aSource
        override def offset = a.offset
      }
    }
  })

  lazy val HereDocument: Parser[String] =
             HeredocStartLine >> { case (sig, rest) =>
               HeredocBody(sig) ~ HeredocEndLine(sig) >> { case b ~ e =>
                 val line = new CharSequenceReader(rest, 0)
                 Parser{next => Success("Heredoc(\"" + b + "\")", concat(line, next))}
               }
             }

//  lazy val HereDocument: Parser[String] =
//             HeredocStartLine >> { case heredocDelimiter => HeredocBody(heredocDelimiter) ~
//                 HeredocEndLine(heredocDelimiter) ^^ { case b~e => "Heredoc(\n"+b+")" } }
  lazy val HeredocStartLine: Parser[(String, String)] =
             HeredocSignifier ~ RestOfLine ^^ { case s~r => (s, r) }
//  lazy val HeredocStartLine: Parser[String] =
//             HeredocSignifier ~ RestOfLine ^^ { case s~r => s }
  lazy val HeredocSignifier: Parser[String] =
             regex("""<<""".r) ~ HeredocDelimiterSpecifier ^^ { case r~s => s }
  lazy val RestOfLine: Parser[String] =
             LineContent ~ LineTerminator ^^ { case c~t => c+"\n" }
  def HeredocBody(heredocDelimiter: String): Parser[String] =
             (HeredocBodyLine(heredocDelimiter)*) ^^ { (_).mkString }
  def HeredocBodyLine(heredocDelimiter: String): Parser[String] =
             not(HeredocEndLine(heredocDelimiter)) ~ LineContent ~ LineTerminator ^^ { case n~c~t => c+"\n" }
  lazy val HeredocDelimiterSpecifier: Parser[String] =
             regex("""\-?""".r) ~ HeredocDelimiter ^^ { case r~d => r+d }
  lazy val HeredocDelimiter: Parser[String] =
             NonQuotedDelimiter |
             SingleQuotedDelimiter |
             DoubleQuotedDelimiter |
             CommandQuotedDelimiter
  lazy val NonQuotedDelimiter: Parser[String] =
             NonQuotedDelimiterIdentifier
  lazy val NonQuotedDelimiterIdentifier: Parser[String] =
             (IdentifierCharacter*) ^^ { (_).mkString }
  lazy val SingleQuotedDelimiter: Parser[String] =
             regex("""'""".r) ~ SingleQuotedDelimiterIdentifier ~ regex("""'""".r) ^^ {
                                   case r1~i~r2 => "'"+i+"'" }
  lazy val SingleQuotedDelimiterIdentifier: Parser[String] =
             ( (not(regex("""'""".r) | LineTerminator) ~ SourceCharacter)*) ^^ { (_).mkString }
  lazy val DoubleQuotedDelimiter: Parser[String] =
             regex(""""""".r) ~ DoubleQuotedDelimiterIdentifier ~ regex(""""""".r) ^^ {
                                   case r1~i~r2 => "\""+i+"\"" }
  lazy val DoubleQuotedDelimiterIdentifier: Parser[String] =
             ( (not(regex(""""""".r) | LineTerminator) ~ SourceCharacter)*) ^^ { (_).mkString }
  lazy val CommandQuotedDelimiter: Parser[String] =
             regex("""`""".r) ~ CommandQuotedDelimiterIdentifier ~ regex("""`""".r) ^^ {
                                   case r1~i~r2 => "`"+i+"`" }
  lazy val CommandQuotedDelimiterIdentifier: Parser[String] =
             ( (not(regex("""`""".r) | LineTerminator) ~ SourceCharacter)*) ^^ { (_).mkString }
  def HeredocEndLine(heredocDelimiter: String): Parser[String] =
             if (heredocDelimiter.head == '-') IndentedHeredocEndLine(heredocDelimiter) else
                   NonIndentedHeredocEndLine(heredocDelimiter)
  def IndentedHeredocEndLine(heredocDelimiter: String): Parser[String] =
             regex("""^""".r) ~ ((Whitespace*) ^^ { (_).mkString }) ~ ( HeredocDelimiterIdentifier ^? {
                 case a if a == heredocDelimiter => a }) ~ LineTerminator ^^ { case r~w~i~t => w+i+"\n" }
  def NonIndentedHeredocEndLine(heredocDelimiter: String): Parser[String] =
             regex("""^""".r) ~ ( HeredocDelimiterIdentifier ^? { case a if a == heredocDelimiter => a }) ~
                 LineTerminator ^^ { case r~i~t => i+"\n" }
  lazy val HeredocDelimiterIdentifier: Parser[String] =
             NonQuotedDelimiterIdentifier |
             SingleQuotedDelimiterIdentifier |
             DoubleQuotedDelimiterIdentifier |
             CommandQuotedDelimiterIdentifier

// 8.7.6.3.7 External command execution

  lazy val ExternalCommandExecution: Parser[String] =
             BackquotedExternalCommandExecution |
             QuotedExternalCommandExecution
  lazy val BackquotedExternalCommandExecution: Parser[String] =
             regex("""`""".r) ~ ((BackquotedExternalCommandExecutionCharacter*) ^^ { (_).mkString } ) ~
                 regex("""`""".r) ^^ { case r1~s~r2 => "`"+s+"`" }
  lazy val BackquotedExternalCommandExecutionCharacter: Parser[String] =
             not(regex("""`|#|\\""".r)) ~ SourceCharacter ^^ { case n~c => c } |
             not(regex("""\$|@|\{""".r)) ~ regex("""#""".r) ^^ { case n~r => r } |
             DoubleEscapeSequence |
             InterpolatedCharacterSequence
  lazy val QuotedExternalCommandExecution: Parser[String] =
             regex("""%x""".r) ~ ExpandedDelimitedString ^^ { case r~n => r+n }

// 8.7.6.4 Array literals

  lazy val ArrayLiteral: Parser[String] =
             QuotedNonExpandedArrayConstructor |
             QuotedExpandedArrayConstructor
  lazy val QuotedNonExpandedArrayConstructor: Parser[String] =
             regex("""%w""".r) ~ ( LiteralBeginningDelimiter >> { case literalBegin =>
                 NonExpandedArrayContent(literalBegin) ~ LiteralEndingDelimiter2(literalBegin) ^^ {  case a~e => literalBegin+a+e  }
             } ) ^^ { case r~s => r+s }
  def NonExpandedArrayContent(literalBegin: String): Parser[String] =
             ??(QuotedArrayItemSeparatorList) ~ ??(NonExpandedArrayItemList(literalBegin)) ~
                 ??(QuotedArrayItemSeparatorList) ^^ { case s1~a~s2 => s1+a+s2 }
  def NonExpandedArrayItemList(literalBegin: String): Parser[String] =
             rep1sep(NonExpandedArrayItem(literalBegin),  QuotedArrayItemSeparatorList) ^^ { (_).mkString("", ", ", "") }
  lazy val QuotedArrayItemSeparatorList: Parser[String] =
             (QuotedArrayItemSeparator+) ^^ {  (_).mkString }
  lazy val QuotedArrayItemSeparator: Parser[String] =
             Whitespace |
             LineTerminator
  def NonExpandedArrayItem(literalBegin: String): Parser[String] =
             (NonExpandedArrayItemCharacter(literalBegin)+) ^^ {  (_).mkString }
  def NonExpandedArrayItemCharacter(literalBegin: String): Parser[String] =
             NonEscapedArrayCharacter(literalBegin) |
             NonExpandedArrayEscapeSequence(literalBegin)
  def NonEscapedArrayCharacter(literalBegin: String): Parser[String] =
             not(QuotedArrayItemSeparator) ~ NonEscapedLiteralCharacter(literalBegin) ^^ { case n~s => s }
  def NonExpandedArrayEscapeSequence(literalBegin: String): Parser[String] =
             NonExpandedLiteralEscapeSequence(literalBegin) |
             regex("""\\""".r) ~ QuotedArrayItemSeparator  ^^ { case r~s => r+s }
  lazy val QuotedExpandedArrayConstructor: Parser[String] =
             regex("""%W""".r) ~ ( LiteralBeginningDelimiter >> { case literalBegin =>
                 ExpandedArrayContent(literalBegin) ~ LiteralEndingDelimiter2(literalBegin) ^^ {
                               case a~e => literalBegin+a+e } } ) ^^ { case r~s => r+s }
  def ExpandedArrayContent(b: String): Parser[String] =
             ??(QuotedArrayItemSeparatorList) ~  ??(ExpandedArrayItemList(b)) ~ ??(QuotedArrayItemSeparatorList) ^^ {
                               case s1~a~s2 => s1+a+s2 }
  def ExpandedArrayItemList(b: String): Parser[String] =
             rep1sep(ExpandedArrayItem(b), QuotedArrayItemSeparatorList) ^^ { (_).mkString("", ", ", "") }
  def ExpandedArrayItem(b: String): Parser[String] =
             (ExpandedArrayItemCharacter(b)+) ^^ {  (_).mkString }
  def ExpandedArrayItemCharacter(b: String): Parser[String] =
             NonEscapedArrayItemCharacter(b) |
             not(regex("""\$|@|\{""".r)) ~ regex("""#""".r) ^^ { case n~r => r } |
             ExpandedArrayEscapeSequence | InterpolatedCharacterSequence
  def NonEscapedArrayItemCharacter(b: String): Parser[String] =
             not(QuotedArrayItemSeparator | regex("""\\|#""".r)) ~ SourceCharacter ^? {
                               case n~s if (b=="(" || b=="{" || b=="[" || b=="<" || b!=s) => s }
  lazy val ExpandedArrayEscapeSequence: Parser[String] =
             DoubleEscapeSequence |
             regex("""\\""".r) ~ QuotedArrayItemSeparator ^^ { case r~s => r+s }

// 8.7.6.5 Regular expression literals

  lazy val RegularExpressionLiteral: Parser[String] =
             regex("""/""".r) ~ RegularExpressionBody ~ regex("""/""".r) ~ ((RegularExpressionOption*) ^^ {
                                    (_).mkString }) ^^ { case r1~b~r2~o => "/"+b+"/"+o } |
             regex("""%r""".r) ~ LiteralBeginningDelimiter >> { case r~literalBegin =>
                 ( (ExpandedLiteralString(literalBegin)*) ^^ { (_).mkString } ) ~
                 LiteralEndingDelimiter2(literalBegin) ~ ((RegularExpressionOption*) ^^ {  (_).mkString }) ^^ {
                                    case s~e~o => "%r"+literalBegin+s+e } }
  lazy val RegularExpressionBody: Parser[String] =
             (RegularExpressionCharacter*) ^^ {  (_).mkString }
  lazy val RegularExpressionCharacter: Parser[String] =
             not(regex("""/|#|\\""".r)) ~ SourceCharacter ^^ { case n~c => c } |
             not(regex("""\$|@|\{""".r)) ~ regex("""#""".r) ^^ { case n~r => r } |
             RegularExpressionNonEscapedSequence |
             RegularExpressionEscapeSequence |
             LineTerminatorEscapeSequence |
             InterpolatedCharacterSequence
  lazy val RegularExpressionNonEscapedSequence: Parser[String] =
             regex("""\\""".r) ~ RegularExpressionNonEscapedCharacter ^^ { case r~c => "\\"+c }
  lazy val RegularExpressionNonEscapedCharacter: Parser[String] =
             not(regex("""\u000d|\u000a""".r)) ~ SourceCharacter ^^ { case n~c => c } |
             regex("""\u000d""".r) ~ not(regex("""\u000a""".r)) ^^ { case d~a => d }
  lazy val RegularExpressionEscapeSequence: Parser[String] =
             regex("""\\/""".r)
  lazy val RegularExpressionOption: Parser[String] =
             regex("""i|m""".r)

// 8.7.6.6 Symbol literals
  lazy val Symbol: Parser[String] =
             SymbolLiteral |
             DynamicSymbol
  lazy val SymbolLiteral: Parser[String] =
             regex(""":""".r) ~ SymbolName ^^ { case r~s => ":"+s }
  lazy val DynamicSymbol: Parser[String] =
             regex(""":""".r) ~ SingleQuotedString ^^ { case r~s => ":"+s } |
             regex(""":""".r) ~ DoubleQuotedString ^^ { case r~s => ":"+s } |
             regex("""%s""".r) ~ LiteralBeginningDelimiter >> { case r~literalBegin =>
                 ( (NonExpandedLiteralString(literalBegin)*) ^^ { (_).mkString } ) ~
                 LiteralEndingDelimiter2(literalBegin) ^^ { case s~e => "%s"+literalBegin+s+e } }
  lazy val SymbolName: Parser[String] =
             InstanceVariableIdentifier |
             GlobalVariableIdentifier |
             ClassVariableIdentifier |
             ConstantIdentifier |
             LocalVariableIdentifier |
             MethodOnlyIdentifier |
             AssignmentLikeMethodIdentifier |
             OperatorMethodName |
             Keyword


}
