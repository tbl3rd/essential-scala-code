package adt.mylist

sealed trait MyOption[A]

case class MySome[A](value: A) extends MyOption[A]

case class MyNone[Nothing]() extends MyOption[Nothing]

sealed trait MyList[+A] {

  def contains[AA >: A](i: AA): Boolean = this match {
    case MyListNil => false
    case MyListPair(head, _) if head == i => true
    case MyListPair(_, tail) => tail contains i
  }


  def exists(f: A => Boolean): Boolean = this match {
    case MyListNil => false
    case MyListPair(head, tail) => f(head) || tail.exists(f)
  }

  def filter(f: A => Boolean): MyList[A] = this match {
    case MyListNil => this
    case MyListPair(head, tail) if f(head) => MyListPair(head, tail.filter(f))
    case MyListPair(_, tail) => tail.filter(f)
  }

  def find[AA >: A](f: AA => Boolean): MyOption[AA] = this match {
    case MyListNil => MyNone[AA]()
    case MyListPair(head, _) if f(head) => MySome(head)
    case MyListPair(_, tail) => tail.find(f)
  }

  // AA is supertype of A
  def append[AA >: A](that: MyList[AA]): MyList[AA] = this match {
    case MyListNil => that
    case MyListPair(head, tail) => MyListPair(head, tail.append(that))
  }

  def map[B](f: A => B): MyList[B] = this match {
    case MyListNil => MyListNil
    case MyListPair(head, tail) => MyListPair(f(head), tail.map(f))
  }

  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case MyListNil => MyListNil
    case MyListPair(head, tail) =>
      val x: MyList[B] = f(head)
      val y: MyList[B] = tail.flatMap(f)
      x.append(y)
  }

}

case object MyListNil extends MyList[Nothing]

case class MyListPair[A](head: A, tail: MyList[A]) extends MyList[A]

object Main extends App {
  val ints = MyListPair(1, MyListPair(2, MyListPair(3, MyListNil)))

  println(ints + """.contains(1) == """ + ints.contains(1))
  println(ints + """.contains(5) == """ + ints.contains(5))

  println(ints.exists(n => n == 2))
  println(ints.exists(n => n == 23))

  println(ints.exists(n => 0 == n % 2))
  println(ints.exists(0 == _ % 2))

  println(ints.filter(_ > 1))
  println(ints.find(_ > 23))
  println(ints.find(_ > 2) match {
    case MyNone() => "None"
    case MySome(n) => n
  })

  println(ints + ".map(_ + 1) ==" + ints.map(_ + 1))

  println(ints + 1.toString)
  println(ints + ".flatMap(List(_.toDouble)) ==" + ints.flatMap(x => MyListPair(x.toDouble, MyListNil)))
}
