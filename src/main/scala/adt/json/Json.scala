package adt.json

sealed trait Json {
  def stringify: String = this match {
    case Jnull() => "null"
    case Jfalse() => "false"
    case Jtrue() => "true"
    case Jnumber(n) => n.toString
    case Jstring(s) => s""""$s""""
    case Jarray(a) => "[" + a.map(x => x.stringify).mkString(", ") + "]"
    case Jobject(o) => "{" + o.map {
      case (k, v) => s""""$k": ${v.stringify}"""
    }.mkString(", ") + "}"
  }
}

case class Jnull() extends Json

case class Jfalse() extends Json

case class Jtrue() extends Json

case class Jnumber(n: Double) extends Json

case class Jstring(s: String) extends Json

case class Jarray(a: List[Json]) extends Json

case class Jobject(o: Map[String, Json]) extends Json

object Main extends App {
  val jt = Jtrue()
  val jf = Jfalse()
  val ja = Jarray(List(0, 1, 2).map { Jnumber(_) })
  val jo = Jobject(Map("jarray" -> ja, "null" -> Jnull(), "string" -> Jstring("jstring"), "b" -> jf))

  println("jt == " + jt.stringify)
  println("ja == " + ja.stringify)
  println("jo == " + jo.stringify)
}
