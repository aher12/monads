package monads

// Type class для монады — параметризован над конструктором типа M[_].
trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  // map выводится из pure + flatMap
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

object Monad:
  // Extension-методы: добавляем .flatMap и .map любому M[A],
  // для которого есть given Monad[M]. Это позволяет писать for-comprehension.
  extension [M[_], A](ma: M[A])(using m: Monad[M])
    def flatMap[B](f: A => M[B]): M[B] = m.flatMap(ma)(f)
    def map[B](f: A => B): M[B]        = m.map(ma)(f)