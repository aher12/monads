package tournament

// Результат одного матча между двумя командами.
// goalsA — голы первой команды, goalsB — голы второй.
final case class MatchResult(
                              teamA:  String,
                              teamB:  String,
                              goalsA: Int,
                              goalsB: Int,
                              round:  Int
                            )

// Запись о команде в турнирной таблице.
// points — набранные очки, goalsScored — забито, goalsConceded — пропущено.
final case class TeamRecord(
                             name:          String,
                             points:        Int,
                             goalsScored:   Int,
                             goalsConceded: Int,
                             wins:          Int,
                             draws:         Int,
                             losses:        Int
                           )

// Результат исхода для команды: победа, ничья, поражение.
enum Outcome:
  case Win, Draw, Loss

// Правило тай-брейка при равенстве очков.
enum TieBreakRule:
  case GoalDifference, GoalsScored, HeadToHead, GoalDiffThenScored

// Неизменяемая конфигурация турнира — Env для Reader-монады.
// pointsForWin/Draw/Loss — очки за исходы.
// maxRounds — максимальное число туров.
// forbidRematch — запрет повторной игры команд в одном туре.
// tieBreak — правило тай-брейка.
final case class TournamentConfig(
                                   pointsForWin:   Int,
                                   pointsForDraw:  Int,
                                   pointsForLoss:  Int,
                                   maxRounds:      Int,
                                   forbidRematch:  Boolean,
                                   tieBreak:       TieBreakRule
                                 )

object TournamentConfig:
  // Конфиг по умолчанию: 3 за победу, 1 за ничью, 0 за поражение.
  // Тай-брейк: разница голов, затем забитые.
  val default: TournamentConfig = TournamentConfig(
    pointsForWin  = 3,
    pointsForDraw = 1,
    pointsForLoss = 0,
    maxRounds     = 10,
    forbidRematch = true,
    tieBreak      = TieBreakRule.GoalDiffThenScored
  )

// Полное состояние турнира — S для State-монады.
// teams — список команд (названия).
// matches — все сыгранные матчи.
// currentRound — текущий тур.
final case class TournamentState(
                                  teams:        List[String],
                                  matches:      List[MatchResult],
                                  currentRound: Int
                                )

object TournamentState:
  // Начальное состояние: пустой список команд, нет матчей, тур 1.
  val empty: TournamentState = TournamentState(
    teams        = List.empty,
    matches      = List.empty,
    currentRound = 1
  )
// Extension-методы для TeamRecord — добавлены снаружи без наследования.
extension (record: TeamRecord)
  // Разница забитых и пропущенных.
  def goalDifference: Int = record.goalsScored - record.goalsConceded

  // Краткое строковое представление для вывода в таблице.
  def summary: String =
    s"${record.name}: ${record.points}pts (W:${record.wins} D:${record.draws} L:${record.losses}) GF:${record.goalsScored} GA:${record.goalsConceded} GD:${goalDifference}"  
  