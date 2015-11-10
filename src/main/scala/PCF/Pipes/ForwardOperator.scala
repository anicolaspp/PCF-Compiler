/**
 * Created by nperez on 11/9/15.
 */

package PCF.Pipes

//There is not forward pipes in scala, so lets implement our owns
// forward pipes can be used as x |> f which is the same as f(x)
// pipes are important to avoid this f(g(y(x))) so we can write x |> y |> g |> f


class Pipe[T](a: T) {
  def |>[B](f: T => B) = {
    f(a)
  }
  def |>() = {}
}

object IPipe {
  implicit def toPipe[T](v: T) = {
    new Pipe[T](v)
  }
}

object Pipe{
  def apply[T](a: T) = new Pipe[T](a)
}
