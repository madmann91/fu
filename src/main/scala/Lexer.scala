package eta

import java.io.InputStream

object Token {
  enum Tag {
    case Eof, Dot, Type, Uni, IntT, FloatT, Lambda, Pi, Let, Rec, LParen, RParen
  }
}

class Token(val tag: Token.Tag, val content: String, val loc: Loc)

class Lexer(val source: InputStream) {
  eat()

  def next() = {
    eatSpaces()
  }

  class Utf8Char(var bytes: Array[Byte]) {
    override def equals(other: Any) = other match {
      case other: Utf8Char => bytes == other.bytes
      case other: Char => bytes(0) == other
      case _ => false
    }
  }

  private var cur: Option[Char] = None
  private def eat() = {
  }
  private def eatSpaces() = {
    while (accept(' ') || accept('\n') || accept('\t'))
      eat()
  }
  private def accept(c: Char) = cur match {
    case Some(ch) if c == ch => {
      eat()
      true
    }
    case _ => false
  }
}

