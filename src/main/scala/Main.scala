object ProbabilisticProgramming {
  implicit class Pmf[A](probabilities: Map[A, Double]) {
    def set(k: A, v: Double): Pmf[A] = probabilities + (k -> v)
    def multiply(k: A, v: Double): Pmf[A] =
      probabilities.get(k) match {
        case Some(w) => probabilities.updated(k, v * w)
        case _       => probabilities
      }

    def normalized: Pmf[A] = {
      val x = probabilities.values.sum
      probabilities.mapValues(_ / x)
    }

    def observe(k: A)(implicit likelihood: (A, A) => Double) =
      probabilities.map { case (h, p) => (h, p * likelihood(h, k)) }.toMap.normalized

    override def toString = probabilities.toString()

    def hist(implicit ord: Ordering[A] = null) =
      if (probabilities.isEmpty) println("impossible")
      else {
        val data = if (ord == null) probabilities.toList else probabilities.toList.sortBy(_._1)
        val scale = 100
        val maxWidth = data.map(_._1.toString.length).max
        val fmt = "%" + maxWidth + "s %s %s"
        data.foreach { case (b, p) =>
          val hashes = (p * scale).toInt
          println(fmt.format(b.toString, f"$p%.2f", "#" * hashes))
        }
      }
  }

  implicit def pmfFromSequence[A](possibilities: Seq[A]): Pmf[A] =
    possibilities.groupBy(identity).map { case (k, as) => k -> as.size.toDouble}.toMap.normalized
}

object Main extends App {
  import ProbabilisticProgramming._

  // val pmf = (1 to 6).foldLeft(Pmf[Int]()) { case (pmf, k) => pmf.set(k, 1.0 / 6) }
  val pmf = (1 to 6).map(_ -> 1.0).toMap.normalized

  val pmf2 = (("Heads" -> 0.5) :: ("Tails" -> 0.5) :: Nil).toMap

  println(pmf)
  println(pmf2)
  println(pmf2("Heads"))

  val pmf3 = pmf2.multiply("Heads", 0.75).multiply("Tails", 0.5).normalized

  println(pmf3)

  // Monty Hall Problem

  implicit val likelihood = (hypo: String, data: String) => hypo match {
    case `data` => 0
    case "A"    => 0.5
    case _      => 1
  }

  Seq("A", "B", "C").observe("B").hist()
}
