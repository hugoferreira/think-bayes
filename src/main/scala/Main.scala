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

    def observe[B](k: B)(implicit likelihood: (A, B) => Double) =
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

  implicit def pmfFromIterable[A](possibilities: Iterable[A]): Pmf[A] =
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
}

object Cookies extends App {
  import ProbabilisticProgramming._

  val mixes = Map('Bowl1 -> Map('vanilla -> 0.75, 'chocolate -> 0.25),
                  'Bowl2 -> Map('vanilla -> 0.5,  'chocolate -> 0.5))

  implicit val likelihood = (hypo: Symbol, data: Symbol) => mixes(hypo)(data)

  println(mixes.keys.observe('vanilla))
}

object MontyHall extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Symbol, data: Symbol) => hypo match {
    case `data` => 0
    case 'A     => 0.5
    case _      => 1
  }

  Seq('A, 'B, 'C).observe('B).hist()
}

object MnM extends App {
  import ProbabilisticProgramming._

  val mix94 = Map('brown -> 30, 'yellow -> 20, 'red -> 20, 'green -> 10, 'orange -> 10, 'tan -> 10)
  val mix96 = Map('blue -> 24, 'green -> 20, 'orange -> 16, 'yellow -> 14, 'red -> 13, 'brown -> 13)

  val hypoA = Map('bag1 -> mix94, 'bag2 -> mix96)
  val hypoB = Map('bag1 -> mix96, 'bag2 -> mix94)

  val hypotheses = Map('A -> hypoA, 'B -> hypoB)

  implicit val likelihood = (hypo: Symbol, data: (Symbol, Symbol)) => {
    val (bag, color) = data
    hypotheses(hypo)(bag)(color).toDouble
  }

  println(hypotheses.keys.observe(('bag1, 'yellow)).observe(('bag2, 'green)))
}
