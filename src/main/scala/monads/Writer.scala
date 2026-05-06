package monads

// Writer[Log, A] — пара из накопленного лога и значения A.
// Лог write-only: можно только добавлять записи.
final case class Writer[Log, A](log: Log, value: A):
  // map не трогает лог.
  def map[B](f: A => B): Writer[Log, B] =
    Writer(log, f(value))

  // Для склеивания логов нужно доказательство, что Log — моноид.
  def flatMap[B](f: A => Writer[Log, B])(using ev: Monoid[Log]): Writer[Log, B] =
    val Writer(log2, b) = f(value)
    Writer(ev.combine(log, log2), b)

// Моноид: empty — нейтральный, combine — ассоциативная операция.
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object Monoid:
  given vectorMonoid[T]: Monoid[Vector[T]] with
    def empty = Vector.empty
    def combine(x: Vector[T], y: Vector[T]) = x ++ y

object Writer:
  // Единственный способ записать в лог.
  def tell[Log](l: Log): Writer[Log, Unit] = Writer(l, ())

  // Нейтральный элемент: пустой лог, значение a.
  def pure[Log, A](a: A)(using ev: Monoid[Log]): Writer[Log, A] =
    Writer(ev.empty, a)

  // given-инстанс только если есть Monoid[Log].
  given writerMonad[Log](using ev: Monoid[Log]): Monad[[A] =>> Writer[Log, A]] with
    def pure[A](a: A) = Writer.pure(a)
    def flatMap[A, B](ma: Writer[Log, A])(f: A => Writer[Log, B]) = ma.flatMap(f)