package monads

// Reader[Env, A] — вычисление, которому нужна среда Env для получения A.
// Среда read-only.
final case class Reader[Env, A](run: Env => A):
  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

  // Одна и та же env передаётся обоим вычислениям.
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

object Reader:
  // Запрашивает всё окружение целиком.
  def ask[Env]: Reader[Env, Env] = Reader(identity)

  // Запрашивает часть окружения через функцию f.
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(f)

  // Нейтральный элемент — окружение игнорируется.
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)

  // given-инстанс монады. [A] =>> Reader[Env, A] — фиксируем Env,
  // оставляем A свободным, чтобы удовлетворить Monad[M[_]].
  given readerMonad[Env]: Monad[[A] =>> Reader[Env, A]] with
    def pure[A](a: A) = Reader.pure(a)
    def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B]) = ma.flatMap(f)