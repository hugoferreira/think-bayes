import scala.collection.immutable.TreeMap

object ProbabilisticProgramming {
  implicit class Pmf[A](probabilities: Map[A, Double]) {
    def set(k: A, v: Double) = probabilities + (k -> v)
    def multiply(k: A, v: Double) =
      probabilities.get(k) match {
        case Some(w) => probabilities.updated(k, v * w)
        case _       => probabilities
      }

    def mean(implicit n: Numeric[A]) = probabilities.foldLeft(0.0) { case (acc, (k, v)) => acc + n.toDouble(k) * v }

    def cdf(implicit ord: Ordering[A]) = probabilities.toList.sortBy(_._1).foldLeft((TreeMap.empty[A, Double], 0.0)) {
      case ((map, cum), (k, p)) => (map + (k -> (cum + p)), cum + p)
    }._1

    def percentile(p: Double)(implicit ord: Ordering[A]): A = cdf.dropWhile { _._2 < p }.head._1
    def credibility(lower: Double, upper: Double)(implicit ord: Ordering[A]): (A, A) = (percentile(lower), percentile(upper))

    def normalized = {
      val x = probabilities.values.sum
      probabilities.mapValues(_ / x)
    }

    def observe[B](obs: B*)(implicit likelihood: (A, B) => Double) =
      obs.foldLeft(probabilities) { case (pmf, o) => pmf.map { case (h, p) => (h, p * likelihood(h, o)) } }.normalized

    override def toString = probabilities.toString()

    def hist(trim: Boolean = false)(implicit ord: Ordering[A] = null) =
      if (probabilities.isEmpty) println("impossible")
      else {
        val data = if (ord == null) probabilities.toList else probabilities.toList.sortBy(_._1)
        val scale = 60
        val keyWidth = data.map(_._1.toString.length).max
        val maxP = data.map(_._2).max
        val fmt = "%" + keyWidth + "s %s %s"
        data.foreach { case (b, p) =>
          val hashes = (p * scale / maxP).toInt
          if (!trim || hashes > 0) println(fmt.format(b.toString, f"$p%.2f", "#" * hashes))
        }
      }
  }

  implicit def fromIterable[A](possibilities: Iterable[A]): Pmf[A] =
    possibilities.groupBy(identity).map { case (k, as) => k -> as.size.toDouble}.toMap.normalized

  object Distributions {
    def powerLaw[A](possibilities: Iterable[A], alpha: Double = 1.0) =
      possibilities.zipWithIndex.map { case (k, ix) => (k, Math.pow((ix + 1).toDouble, -alpha)) }.toMap.normalized
  }
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

  val hypoA = Map('vanilla -> 0.75, 'chocolate -> 0.25)
  val hypoB = Map('vanilla -> 0.5,  'chocolate -> 0.5)
  val hypotheses = Map('Bowl1 -> hypoA, 'Bowl2 -> hypoB)

  implicit val likelihood = hypotheses(_: Symbol)(_: Symbol)

  Seq('Bowl1, 'Bowl2).observe('vanilla).hist()
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

  Seq('A, 'B).observe(('bag1, 'yellow)).observe(('bag2, 'green)).hist()
}

object DnD extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Int) => if (hypo < data) 0.0 else 1.0 / hypo
  val posterior = Seq(4, 6, 8, 12, 20).observe(6, 6, 8, 7, 7, 5, 4)

  posterior.hist()
  println("------------")
  posterior.cdf.hist()
}

object Locomotive extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Int) => if (hypo < data) 0.0 else 1.0 / hypo
  val posteriorA = (1 to 1000).observe(60)
  val posteriorB = (1 to 1000).observe(60, 30, 90)

  println(s"p(n|60) = ${posteriorA.get(60)}")
  println(s"mean of the posterior: ${posteriorA.mean}")
  println(s"p(n=60|60,30,90) = ${posteriorB.get(60)}")
  println(s"p(n=90|60,30,90) = ${posteriorB.get(90)}")
  println(s"mean of the posterior: ${posteriorB.mean}")

  println(s"Credible Interval (0.05, 0.95) = ${posteriorB.credibility(0.05, 0.95)}")
}

object Locomotive2 extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Int) => if (hypo < data) 0.0 else 1.0 / hypo
  val hypotheses = Distributions.powerLaw(1 to 1000)
  val posteriorA = hypotheses.observe(60)
  val posteriorB = hypotheses.observe(60, 30, 90)

  println(s"p(n|60) = ${posteriorA.get(60)}")
  println(s"mean of the posterior: ${posteriorA.mean}")
  println(s"p(n=60|60,30,90) = ${posteriorB.get(60)}")
  println(s"p(n=90|60,30,90) = ${posteriorB.get(90)}")
  println(s"mean of the posterior: ${posteriorB.mean}")

  println(s"Credible Interval (0.05, 0.95) = ${posteriorB.credibility(0.05, 0.95)}")
}

object Euro extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Symbol) => if (data == 'H) hypo / 100.0 else 1 - hypo / 100.0

  val prior = 0 to 100
  val posterior = prior.observe((1 to 140).map(_ => 'H) ++ (1 to 110).map(_ => 'T) : _*)

  posterior.hist(trim = true)
}