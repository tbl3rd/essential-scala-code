package adt.mylist

sealed trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }
}

case class MySome[A](value: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]


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

  def find[AA >: A](f: AA => Boolean): MyOption[A] = this match {
    case MyListNil => MyNone
    case MyListPair(head, _) if f(head) => MySome(head)
    case MyListPair(_, tail) => tail.find(f)
  }

  def map[B](f: A => B): MyList[B] = this match {
    case MyListNil => MyListNil
    case MyListPair(head, tail) => MyListPair(f(head), tail.map(f))
  }

  def append[AA >: A](that: MyList[AA]): MyList[AA] = this match {
    case MyListNil => that
    case MyListPair(head, tail) => MyListPair(head, tail.append(that))
  }

  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case MyListNil => MyListNil
    case MyListPair(head, tail) => f(head).append(tail.flatMap(f))
  }

  def foldLeft[B](empty: B)(f: (B, A) => B): B = this match {
    case MyListNil => empty
    case MyListPair(head, tail) => tail.foldLeft(f(empty, head))(f)
  }

  def foldRight[B](empty: B)(f: (A, B) => B): B = this match {
    case MyListNil => empty
    case MyListPair(head, tail) => f(head, tail.foldRight(empty)(f))
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
    case MyNone => "None"
    case MySome(n) => n
  })

  val charsOf: String => MyList[Char] = (s: String) => s match {
    case "" => MyListNil
    case _ => MyListPair(s.charAt(0), charsOf(s.substring(1)))
  }

  val words = MyListPair[String]("a", MyListPair("few", MyListPair("words", MyListNil)))
  println(words.flatMap(charsOf))
}
