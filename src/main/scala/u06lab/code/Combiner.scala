package u06lab.code

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double =
    a.foldRight(0.0)((h1, h2) => h1 + h2)
  override def concat(a: Seq[String]): String =
    a.foldRight("")((h1, h2) => h1 ++ h2)
  override def max(a: List[Int]): Int =
    a.foldRight(Integer.MIN_VALUE)((h1, h2) => if h1 > h2 then h1 else h2)


/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

trait Functions2:
  def sum[A](a: List[A])(using summer: Combiner[A]): A
  def concat[A](a: Seq[A])(using summer: Combiner[A]): A
  def max[A](a: List[A])(using summer: Combiner[A]): A

object FunctionsImpl2 extends Functions2:
  override def sum[A](a: List[A])(using summer: Combiner[A]): A =
    a.foldRight(summer.unit)((h1, h2) => summer.combine(h1, h2))
  override def concat[A](a: Seq[A])(using summer: Combiner[A]): A =
    a.foldRight(summer.unit)((h1, h2) => summer.combine(h1, h2))
  override def max[A](a: List[A])(using summer: Combiner[A]): A =
    a.foldRight(summer.unit)((h1, h2) => summer.combine(h1, h2))


given Combiner[Double] with
  override def unit: Double = 0.0
  override def combine(a: Double, b: Double): Double = a + b

given Combiner[String] with
  override def unit: String = ""
  override def combine(a: String, b: String): String = a ++ b

given Combiner[Int] with
  override def unit: Int = Integer.MIN_VALUE
  override def combine(a: Int, b: Int): Int = if a > b then a else b



@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
//  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
//  println(f.sum(List())) // 0.0
//  println(f.concat(Seq("a", "b", "c"))) // abc
//  println(f.concat(Seq())) // ""
//  println(f.max(List(-10, 3, -5, 0))) // 3
//  println(f.max(List())) // -2147483648

  val f2: Functions2 = FunctionsImpl2
  println(f2.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f2.sum(List[Double]())) // 0.0
  println(f2.concat(Seq("a", "b", "c"))) // abc
  println(f2.concat(Seq[String]())) // ""
  println(f2.max(List(-10, 3, -5, 0))) // 3
  println(f2.max(List[Int]())) // -2147483648
