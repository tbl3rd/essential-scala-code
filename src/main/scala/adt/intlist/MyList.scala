package adt.mylist

sealed trait IntOption

case class IntSome(i: Int) extends IntOption

case class IntNone() extends IntOption

sealed trait MyList[A] {

  def contains(i: A): Boolean = this match {
    case MyListNil() => false
    case MyListPair(head, _) if head == i => true
    case MyListPair(_, tail) => tail contains i
  }


  def exists(f: A => Boolean): Boolean = this match {
    case MyListNil() => false
    case MyListPair(head, tail) => f(head) || tail.exists(f)
  }

  def filter(f: A => Boolean): MyList[A] = this match {
    case MyListNil() => this
    case MyListPair(head, tail) if f(head) => MyListPair(head, tail.filter(f))
    case MyListPair(_, tail) => tail.filter(f)
  }

  def find(f: A => Boolean): Option[A] = this match {
    case MyListNil() => None
    case MyListPair(head, _) if f(head) => Some(head)
    case MyListPair(_, tail) => tail.find(f)
  }
}

case class MyListNil[A]() extends MyList[A]

case class MyListPair[A](head: A, tail: MyList[A]) extends MyList[A]

object Main extends App {

  val ints = MyListPair(1, MyListPair(2, MyListPair(3, MyListNil())))

  println(ints + """.contains(1) == """ + ints.contains(1))
  println(ints + """.contains(5) == """ + ints.contains(5))


  println(ints.exists(n => n == 2))
  println(ints.exists(n => n == 23))

  println(ints.exists(n => 0 == n % 2))
  println(ints.exists(0 == _ % 2))

  println(ints.filter(_ > 1))
  println(ints.find(_ > 23))
  println(ints.find(_ > 2) match {
    case None => "None"
    case Some(n) => n
  })
}
