package tournament

trait TournamentReader[F[_]]:
  def pointsFor(outcome: Outcome): F[Int]
  def ranking(teams: List[String], matches: List[MatchResult]): F[List[TeamRecord]]
  def canSchedule(teamA: String, teamB: String, round: Int, matches: List[MatchResult]): F[Boolean]
  def tieBreak(recordA: TeamRecord, recordB: TeamRecord): F[Int]

trait TournamentWriter[F[_]]:
  def tell(msg: String): F[Unit]

trait TournamentStateOps[F[_]]:
  def addTeam(name: String): F[Boolean]
  def recordMatch(result: MatchResult): F[Option[MatchResult]]
  def nextRound: F[Int]
  def resetTournament: F[TournamentState]
  def getTable: F[List[TeamRecord]]