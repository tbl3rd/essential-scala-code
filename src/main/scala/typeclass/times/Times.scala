package typeclass.times

object TimesImplicits {
  implicit class Times(i: Int) {
    def times[A](f: Int => A): List[A] = (1 to i).map(f).toList
  }
}

object Main extends App {
  import TimesImplicits._

   println("""5.times(_ * 10)  == """ + 5.times(_ * 10))
   println("""3.times(_ + "!") == """ + 3.times(_ + "!"))
}
