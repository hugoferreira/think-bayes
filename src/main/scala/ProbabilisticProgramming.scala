import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

object ProbabilisticProgramming {
  implicit class Pmf[A](probabilities: Map[A, Double]) {
    def set(k: A, v: Double) = probabilities + (k -> v)
    def multiply(k: A, v: Double) = probabilities.get(k) match {
      case Some(w) => probabilities.updated(k, v * w)
      case _       => probabilities
    }

    def mean(implicit n: Numeric[A]) = probabilities.foldLeft(0.0) { case (acc, (k, v)) => acc + n.toDouble(k) * v }

    def cdf(implicit ord: Ordering[A]) = probabilities.toList.sortBy(_._1).foldLeft((TreeMap.empty[A, Double], 0.0)) {
      case ((map, cum), (k, p)) => (map + (k -> (cum + p)), cum + p)
    }._1

    // This would probably benefit from a binary search, considering CDF is ordered on both the key and value
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

    def samples: Stream[A] = {
      val len = probabilities.size
      val scale = len / probabilities.values.sum
      val scaled = probabilities.mapValues(_ * scale).toList
      val (small, large) = scaled.partition { _._2 < 1.0 }

      @tailrec
      def alias(small: List[(A, Double)], large: List[(A, Double)], rest: List[(A, Double, Option[A])]): List[(A, Double, Option[A])] = {
        (small, large) match {
          case ((s, ps) :: ss, (l, pl) :: ll) =>
            val remainder = (l, pl - (1.0 - ps))
            val newRest = (s, ps, Some(l)) :: rest
            if (remainder._2 < 1)
              alias(remainder :: ss, ll, newRest)
            else
              alias(ss, remainder :: ll, newRest)
          case (_, (l, _) :: ll) =>
            alias(small, ll, (l, 1.0, None) :: rest)
          case ((s, _) :: ss, _) =>
            alias(ss, large, (s, 1.0, None) :: rest)
          case _ =>
            rest
        }
      }

      val table = Vector() ++ alias(small, large, Nil)
      def select(p1: Double, p2: Double, table: Vector[(A, Double, Option[A])]): A = {
        table((p1 * len).toInt) match {
          case (a, _, None) => a
          case (a, p, Some(b)) => if (p2 <= p) a else b
        }
      }

      import scala.util.Random._

      Stream.continually(select(nextDouble(), nextDouble(), table))
    }
  }

  implicit def fromIterable[A](possibilities: Iterable[A]): Pmf[A] =
    possibilities.groupBy(identity).map { case (k, as) => k -> as.size.toDouble}.toMap.normalized

  object Distributions {
    def inversePowerLaw[A](possibilities: Iterable[A], alpha: Double = 1.0) =
      possibilities.zipWithIndex.map { case (k, ix) => (k, Math.pow((ix + 1).toDouble, -alpha)) }.toMap.normalized

    def uniform[A](possibilities: Iterable[A]) = fromIterable(possibilities)
  }
}