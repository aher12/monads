package tournament

final case class MatchResult(
                              teamA:  String,
                              teamB:  String,
                              goalsA: Int,
                              goalsB: Int,
                              round:  Int
                            )

final case class TeamRecord(
                             name:          String,
                             points:        Int,
                             goalsScored:   Int,
                             goalsConceded: Int,
                             wins:          Int,
                             draws:         Int,
                             losses:        Int
                           )

enum Outcome:
  case Win, Draw, Loss

enum TieBreakRule:
  case GoalDifference, GoalsScored, HeadToHead, GoalDiffThenScored

final case class TournamentConfig(
                                   pointsForWin:   Int,
                                   pointsForDraw:  Int,
                                   pointsForLoss:  Int,
                                   maxRounds:      Int,
                                   forbidRematch:  Boolean,
                                   tieBreak:       TieBreakRule
                                 )

object TournamentConfig:
  val default: TournamentConfig = TournamentConfig(
    pointsForWin  = 3,
    pointsForDraw = 1,
    pointsForLoss = 0,
    maxRounds     = 10,
    forbidRematch = true,
    tieBreak      = TieBreakRule.GoalDiffThenScored
  )

final case class TournamentState(
                                  teams:        List[String],
                                  matches:      List[MatchResult],
                                  currentRound: Int
                                )

object TournamentState:
  val empty: TournamentState = TournamentState(
    teams        = List.empty,
    matches      = List.empty,
    currentRound = 1
  )