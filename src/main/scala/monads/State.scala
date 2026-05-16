package monads

final case class State[S, A](run: S => (S, A)):
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      f(a).run(s1)
    }

object State:
  def inspect[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A) = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]) = ma.flatMap(f)