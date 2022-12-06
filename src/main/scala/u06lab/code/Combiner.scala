package u06lab.code

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = a.foldLeft(0.0)(_+_)
  override def concat(a: Seq[String]): String = a.foldLeft("")(_+_)
  override def max(a: List[Int]): Int = a.foldRight(Int.MinValue)((el, acc) => if el > acc then el else acc)

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

//Combiner is TYPE CLASS since it says how to add operation to the types (Double, String,...)
case object SumCombiner extends Combiner[Double]:
  override def unit = 0.0
  override def combine(a: Double, b: Double): Double = a + b


case object ConcatCombiner extends Combiner[String] :
  override def unit = ""
  override def combine(a: String, b: String): String = a + b


case object MaxCombiner extends Combiner[Int] :
  override def unit = Int.MinValue
  override def combine(a: Int, b: Int): Int = if a > b then a else b



object FunctionsImpl2 extends Functions:
  override def sum(a: List[Double]): Double = combine(a)(SumCombiner)
  override def concat(a: Seq[String]): String = combine(a)(ConcatCombiner)
  override def max(a: List[Int]): Int = combine(a)(MaxCombiner)
  def combine[A](list: Iterable[A])(combiner: Combiner[A]): A = list.foldLeft(combiner.unit)((acc, el)=>combiner.combine(acc,el))

//take the Combiner implicitly
object ImplicitCombiners:
  given Combiner[Double] with
    override def unit = 0.0
    override def combine(a: Double, b: Double): Double = a + b


  given Combiner[String] with
    override def unit = ""
    override def combine(a: String, b: String): String = a + b


  given Combiner[Int] with
    override def unit = Int.MinValue
    override def combine(a: Int, b: Int): Int = if a > b then a else b

object FunctionsImpl3 extends Functions:
  import ImplicitCombiners.given
  override def sum(a: List[Double]): Double = combine(a)
  override def concat(a: Seq[String]): String = combine(a)
  override def max(a: List[Int]): Int = combine(a)
  def combine[A](list: Iterable[A])(using combiner: Combiner[A]): A = list.foldLeft(combiner.unit)((acc, el)=>combiner.combine(acc,el))
  //messo "using" nel metodo combine, sono forzato a mettere la keyword using quando chiamo il metodo
  //  override def sum(a: List[Double]): Double = combine(a)(using SumCombiner)
  //importo gli ImplicitCombiners e posso chiamare il metodo senza secondo argomento

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648

  //using combiner
  val f2: Functions = FunctionsImpl2
  println(f2.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f2.sum(List())) // 0.0
  println(f2.concat(Seq("a", "b", "c"))) // abc
  println(f2.concat(Seq())) // ""
  println(f2.max(List(-10, 3, -5, 0))) // 3
  println(f2.max(List())) // -2147483648

  //using combiner implicitly
  val f3: Functions = FunctionsImpl3
  println(f3.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f3.sum(List())) // 0.0
  println(f3.concat(Seq("a", "b", "c"))) // abc
  println(f3.concat(Seq())) // ""
  println(f3.max(List(-10, 3, -5, 0))) // 3
  println(f3.max(List())) // -2147483648