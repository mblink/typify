package typify

case class ParseError[P](cursor: Cursor[P], message: String)
