package tournament

import monads.{Writer, Monoid}
import monads.Monoid.given

// Log — псевдоним типа. Все функции работают с Writer[Vector[String], A].
type Log = Vector[String]

object WriterOps:

  // Приватный хелпер: оборачивает одну строку в Vector и вызывает Writer.tell.
  private def tell(msg: String): Writer[Log, Unit] =
    Writer.tell(Vector(msg))

  // Логирует запись результата матча, возвращает результат как значение.
  def logMatchResult(result: MatchResult): Writer[Log, MatchResult] =
    for
      _ <- tell(
        s"Round ${result.round}: ${result.teamA} ${result.goalsA} - ${result.goalsB} ${result.teamB}"
      )
    yield result

  // Логирует начисление очков команде, возвращает команду и исход.
  def logPointsAwarded(
                        team:    String,
                        outcome: Outcome,
                        points:  Int
                      ): Writer[Log, (String, Outcome, Int)] =
    for
      _ <- tell(s"Team '$team' awarded $points pts for $outcome")
    yield (team, outcome, points)

  // Логирует изменение позиции в таблице, возвращает команду и новые позиции.
  def logPositionChange(
                         team:   String,
                         oldPos: Int,
                         newPos: Int
                       ): Writer[Log, (String, Int, Int)] =
    if oldPos != newPos then
      for
        _ <- tell(s"Team '$team' moved: #$oldPos -> #$newPos")
      yield (team, oldPos, newPos)
    else
      Writer.pure((team, oldPos, newPos))