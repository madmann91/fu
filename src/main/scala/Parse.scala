package eta

class ParseError(val reason: String, val loc: Loc)
class ParseContext(val lexer: Lexer)

trait Parser[T] {
  def parse(ctx: ParseContext): Either[ParseError, T]
}
