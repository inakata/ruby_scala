package jp.scala_users.org.ruby_scala

import java.io.FileReader
object RubyParser extends Ruby {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(program, reader))
  }
}
