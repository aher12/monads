package tournament

object TournamentLogic:

  def outcomeFor(team: String, m: MatchResult): Outcome =
    val (scored, conceded) =
      if m.teamA == team then (m.goalsA, m.goalsB)
      else (m.goalsB, m.goalsA)
    if scored > conceded then Outcome.Win
    else if scored == conceded then Outcome.Draw
    else Outcome.Loss

  def goalsFor(team: String, m: MatchResult): (Int, Int) =
    if m.teamA == team then (m.goalsA, m.goalsB)
    else (m.goalsB, m.goalsA)

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

  def positionOf(team: String, table: List[TeamRecord]): Int =
    table.indexWhere(_.name == team) + 1