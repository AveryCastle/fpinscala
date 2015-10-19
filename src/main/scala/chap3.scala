package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](as: List[A], a: A): List[A] =
    Cons(a, tail(as))

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n == 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil 
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l 

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  } 
}

case class Point(x: Int, y: Int) 

object Test {

  // TODO: Test 코드로 변경하여 테스트하기
  def main(args: Array[String]) {  
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println(list)
    println(List.sum(list))
    println(List.tail(list)) 
    println(List.tail(Nil)) 
    println(List.setHead(list, 10)) 
    println(List.drop(list, 3)) 
    println(List.drop(list, 10)) 
    println(List.dropWhile(list, (x: Int) => x < 4)) 
    println(List.dropWhile(list, (x: Int) => x < 10)) 

    val point = Point(3, 2)

    println(List.init(list))
  }
}
