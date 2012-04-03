package jp.scala_users.org.ruby_scala

import scala.util.parsing.combinator._
import scala.util.parsing.input._

trait TracableParsers extends Parsers {self =>
  private lazy val names: Map[Parser[Any], String] = {
    val rules = this.getClass().getMethods().filter{m => m.getParameterTypes().length == 0 && (m.getReturnType() eq classOf[Parser[_]])}
    rules.foldLeft(Map[Parser[_], String]()){(m, r) => m.updated(r.invoke(self).asInstanceOf[Parser[_]], r.getName())}
  }
  class Parser[+T](wrapped: super.Parser[T]) extends super.Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val name = names.get(this).getOrElse("?")
      val flag = false // name.head.isLower && name != "anySpace"
      if (flag) println("app " + name + ":" + in.pos)
      val result = wrapped(in)
      if (flag) {
        print("ret " + name +" "+ result.toString+";")
        result match {
          case Failure(_, next) => println(" Failed: " + next.pos)
          case Success(_, next)  => println(" Succeeded: " + next.pos)
        }
      }
      result
    }
  }
  implicit def toTracable[T](wrapped: super.Parser[T]): Parser[T] = {
    new Parser[T](wrapped)
  }
}
