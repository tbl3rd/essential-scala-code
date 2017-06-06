package adt.intlist

sealed trait IntOption

case class IntSome(i: Int) extends IntOption

case class IntNone() extends IntOption

sealed trait IntList {

  def contains(i: Int): Boolean = this match {
    case IntNil() => false
    case IntPair(head, _) if head == i => true
    case IntPair(_, tail) => tail contains i
  }

  def add(i: Int): IntList = this match {
    case IntNil() => this
    case IntPair(head, tail) => IntPair(head + i, tail.add(i))
  }

  def total: Int = this match {
    case IntNil() => 0
    case IntPair(head, tail) => head + tail.total
  }

  def exists(f: Int => Boolean): Boolean = this match {
    case IntNil() => false
    case IntPair(head, tail) => f(head) || tail.exists(f)
  }

  def filter(f: Int => Boolean): IntList = this match {
    case IntNil() => this
    case IntPair(head, tail) if f(head) => IntPair(head, tail.filter(f))
    case IntPair(_, tail) => tail.filter(f)
  }

  def find(f: Int => Boolean): IntOption = this match {
    case IntNil() => IntNone()
    case IntPair(head, _) if f(head) => IntSome(head)
    case IntPair(_, tail) => tail.find(f)
  }
}

case class IntNil() extends IntList

case class IntPair(head: Int, tail: IntList) extends IntList

object Main extends App {

  val ints = IntPair(1, IntPair(2, IntPair(3, IntNil())))

  println(ints + """.contains(1) == """ + ints.contains(1))
  println(ints + """.contains(5) == """ + ints.contains(5))

  println(ints + """.add(1) == """ + ints.add(1))
  println(ints + """.add(5) == """ + ints.add(5))

  println(ints + """.total == """ + ints.total)

  println(ints.exists(n => n == 2))
  println(ints.exists(n => n == 23))

  println(ints.exists(n => 0 == n % 2))
  println(ints.exists(0 == _ % 2))

  println(ints.filter(_ > 1))
  println(ints.find(_ > 23))
  println(ints.find(_ > 2) match {
    case IntNone() => "None"
    case IntSome(n) => n
  })
}
