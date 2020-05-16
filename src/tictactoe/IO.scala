package tictactoe


case class IO[A](run: () => A) { self =>
  def create(a: A): IO[A] = IO(() => a)

  def map[B](f: A => B): IO[B] = IO(() => f(self.run()))

  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.run()).run())
}

object IO {
  def create[A](a: A): IO[A] = IO(() => a)
}
