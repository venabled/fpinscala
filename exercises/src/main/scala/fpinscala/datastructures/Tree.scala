package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum[A](t: Tree[A]): A = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]):Int = t match {
    case Leaf(a) => 0
    case Branch(l, r) => 1 + depth(l) + depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match{
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match{
    case Leaf(a) => l(a)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(a => 1)((l, r) => 1 + l + r )

}
