package monads

// State[S, A] — функция S => (S, A). Берёт старое состояние,
// возвращает новое и значение. Чистая альтернатива var.
final case class State[S, A](run: S => (S, A)):
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

  // Состояние передаётся по цепочке: s -> (s1, a) -> f(a).run(s1).
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      f(a).run(s1)
    }

object State:
  // Читает состояние, не меняя его.
  def get[S]: State[S, S] = State(s => (s, s))

  // Заменяет состояние.
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

  // Преобразует состояние функцией.
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

  // Нейтральный элемент — состояние не меняется.
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  // Читает часть состояния через f.
  def inspect[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))

  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A) = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]) = ma.flatMap(f)