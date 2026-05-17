package tournament

object TournamentLogic:

  // Определение исхода для команды
  def outcomeFor(team: String, m: MatchResult): Outcome =
    val (scored, conceded) =
      if m.teamA == team then (m.goalsA, m.goalsB)
      else (m.goalsB, m.goalsA)
    if scored > conceded then Outcome.Win
    else if scored == conceded then Outcome.Draw
    else Outcome.Loss

  // Счёт голов команды в матче
  def goalsFor(team: String, m: MatchResult): (Int, Int) =
    if m.teamA == team then (m.goalsA, m.goalsB)
    else (m.goalsB, m.goalsA)

  // Единый расчёт турнирной таблицы
  def computeStandings(
                        teams:         List[String],
                        matches:       List[MatchResult],
                        pointsForWin:  Int,
                        pointsForDraw: Int,
                        pointsForLoss: Int
                      ): List[TeamRecord] =
    teams.map { team =>
      val teamMatches = matches.filter(m => m.teamA == team || m.teamB == team)
      val empty = TeamRecord(team, 0, 0, 0, 0, 0, 0)

      val updates: List[TeamRecord => TeamRecord] = teamMatches.map { m =>
        val (s, c) = goalsFor(team, m)
        val outcome = outcomeFor(team, m)
        val earned = outcome match
          case Outcome.Win  => pointsForWin
          case Outcome.Draw => pointsForDraw
          case Outcome.Loss => pointsForLoss

        (r: TeamRecord) =>
          TeamRecord(
            name          = r.name,
            points        = r.points + earned,
            goalsScored   = r.goalsScored + s,
            goalsConceded = r.goalsConceded + c,
            wins          = if outcome == Outcome.Win then r.wins + 1 else r.wins,
            draws         = if outcome == Outcome.Draw then r.draws + 1 else r.draws,
            losses        = if outcome == Outcome.Loss then r.losses + 1 else r.losses
          )
      }

      if updates.isEmpty then empty
      else updates.reduce((f1, f2) => f1.andThen(f2))(empty)
    }

  // Позиция команды в таблице (с 1)
  def positionOf(team: String, table: List[TeamRecord]): Int =
    table.indexWhere(_.name == team) + 1

  // Сортировка по тай-брейку
  def sortWithTieBreak(records: List[TeamRecord], cfg: TournamentConfig): List[TeamRecord] =
    records.sortWith { (a, b) =>
      if a.points != b.points then b.points < a.points
      else
        cfg.tieBreak match
          case TieBreakRule.GoalDifference =>
            (b.goalsScored - b.goalsConceded) > (a.goalsScored - a.goalsConceded)
          case TieBreakRule.GoalsScored =>
            b.goalsScored > a.goalsScored
          case TieBreakRule.HeadToHead => false
          case TieBreakRule.GoalDiffThenScored =>
            val diffA = a.goalsScored - a.goalsConceded
            val diffB = b.goalsScored - b.goalsConceded
            if diffA != diffB then diffB > diffA
            else b.goalsScored > a.goalsScored
    }