package monads
final case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def delay[A](a: => A): IO[A] = IO(() => a)
  def putStrLn(s: String): IO[Unit] = IO(() => println(s))
  val getStrLn: IO[String] = IO(() => Option(scala.io.StdIn.readLine()).getOrElse(""))

  given ioMonad: Monad[IO] with
    def pure[A](a: A) = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]) = ma.flatMap(f)