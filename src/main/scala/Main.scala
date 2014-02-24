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

  val posterior = Seq('A, 'B, 'C).observe('B)
  posterior.hist()
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
  println(f"mean of the posterior: ${posteriorA.mean}%.2f")
  println(s"p(n=60|60,30,90) = ${posteriorB.get(60)}")
  println(s"p(n=90|60,30,90) = ${posteriorB.get(90)}")
  println(f"mean of the posterior: ${posteriorB.mean}%.2f")

  println(s"Credible Interval (0.05, 0.95) = ${posteriorB.credibility(0.05, 0.95)}")
}

object Locomotive2 extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Int) => if (hypo < data) 0.0 else 1.0 / hypo
  val hypotheses = Distributions.inversePowerLaw(1 to 1000)
  val posteriorA = hypotheses.observe(60)
  val posteriorB = hypotheses.observe(60, 30, 90)

  println(s"p(n|60) = ${posteriorA.get(60)}")
  println(f"mean of the posterior: ${posteriorA.mean}%.2f")
  println(s"p(n=60|60,30,90) = ${posteriorB.get(60)}")
  println(s"p(n=90|60,30,90) = ${posteriorB.get(90)}")
  println(f"mean of the posterior: ${posteriorB.mean}%.2f")

  println(s"Credible Interval (0.05, 0.95) = ${posteriorB.credibility(0.05, 0.95)}")
}

object Euro extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Symbol) => if (data == 'H) hypo / 100.0 else 1 - hypo / 100.0

  val prior = 0 to 100
  val posterior = prior.observe((1 to 140).map(_ => 'H) ++ (1 to 110).map(_ => 'T) : _*)

  posterior.hist(trim = true)
  println(f"Mean = ${posterior.mean}%.2f")
  println(s"Median = ${posterior.percentile(0.5)}")
  println(s"Credible Interval (0.05, 0.95) = ${posterior.credibility(0.05, 0.95)}")
}

object EuroSwamped extends App {
  import ProbabilisticProgramming._

  implicit val likelihood = (hypo: Int, data: Symbol) => if (data == 'H) hypo / 100.0 else 1 - hypo / 100.0

  val prior = (0 to 100).map { i => i -> (if (i <= 5) i.toDouble else 100.0 - i) }.toMap

  val posterior = prior.observe((1 to 140).map(_ => 'H) ++ (1 to 110).map(_ => 'T) : _*)

  posterior.hist(trim = true)
  println(f"Mean = ${posterior.mean}%.2f")
  println(s"Median = ${posterior.percentile(0.5)}")
  println(s"Credible Interval (0.05, 0.95) = ${posterior.credibility(0.05, 0.95)}")
  println("------------")
  posterior.cdf.hist(trim = false)
}