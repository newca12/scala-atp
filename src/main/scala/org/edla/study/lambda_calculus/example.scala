//https://www.reddit.com/r/scala/comments/3s7iyh/multivariable_lambda_calculus_in_the_scala_type/
import org.edla.study.lambda_calculus.lambda._

object example extends App {

  println(s"${eval[Plus#ap[_1]#ap[_1]]} == _2")
  type F = (k ->: ((_2 * k) + _1))
  println(s"${eval[F]} == F")
  println(s"${eval[F#ap[_3]]} == _7")
  println(s"${eval[F#ap[_2]]} == _5")

}
