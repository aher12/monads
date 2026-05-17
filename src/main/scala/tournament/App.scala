package tournament

import monads.{IO, Writer, State}
import monads.Monoid.given
import monads.IO.given

object App extends UserInteraction:

  private val reader = ReaderOps.interpreter
  private val writer = WriterOps.interpreter
  private val state  = StateOps.interpreter

  private def print(s: String): IO[Unit] = IO.putStrLn(s)

  private def prompt(msg: String): IO[String] =
    for _ <- print(msg); line <- IO.getStrLn yield line

  private def promptInt(msg: String): IO[Int] =
    prompt(msg).map(_.toInt)

  private def printTable(records: List[TeamRecord]): IO[Unit] =
    if records.isEmpty then print("No teams.")
    else
      val header    = "| Pos | Team           | Pts | W | D | L | GF | GA | GD |"
      val separator = "|-----|----------------|-----|---|---|---|---|----|----|----|"
      val lines = records.zipWithIndex.map { (r, i) =>
        f"| ${i + 1}%3d | ${r.name}%-14s | ${r.points}%3d | ${r.wins}%1d | ${r.draws}%1d | ${r.losses}%1d | ${r.goalsScored}%2d | ${r.goalsConceded}%2d | ${r.goalsScored - r.goalsConceded}%3d |"
      }
      print((header :: separator :: lines).mkString("\n"))

  private def printMatches(matches: List[MatchResult]): IO[Unit] =
    if matches.isEmpty then print("No matches.")
    else
      val lines = matches.map(m => s"Round ${m.round}: ${m.teamA} ${m.goalsA} - ${m.goalsB} ${m.teamB}")
      print(lines.mkString("\n"))

  // --- Логирование ---

  private def logMatch(m: MatchResult): IO[Unit] =
    val (log1, _) = writer.tell(s"Match recorded: ${m.teamA} ${m.goalsA} - ${m.goalsB} ${m.teamB} (round ${m.round})").log -> ()
    IO.delay(log1.foreach(println))

  private def logPoints(team: String, outcome: Outcome, points: Int): IO[Unit] =
    val (log1, _) = writer.tell(s"Team '$team' awarded $points pts for $outcome").log -> ()
    IO.delay(log1.foreach(println))

  private def logPosition(team: String, oldPos: Int, newPos: Int): IO[Unit] =
    if oldPos != newPos then
      val (log1, _) = writer.tell(s"Team '$team' moved: #$oldPos -> #$newPos").log -> ()
      IO.delay(log1.foreach(println))
    else IO.pure(())

  // --- IO-сценарии ---

  private def doAddTeam(current: TournamentState): IO[TournamentState] =
    for
      name       <- prompt("Team name: ")
      (st, added) = state.addTeam(name).run(current)
      _          <- if added then print(s"Team '$name' added.")
      else print(s"Team '$name' already exists.")
    yield st

  private def doRecordMatch(cfg: TournamentConfig, current: TournamentState): IO[TournamentState] =
    if current.teams.size < 2 then print("Need at least 2 teams.").map(_ => current)
    else
      for
        _       <- print(s"Teams: ${current.teams.mkString(", ")}")
        teamA   <- prompt("Team A: ")
        teamB   <- prompt("Team B: ")
        goalsA  <- promptInt(s"Goals for $teamA: ")
        goalsB  <- promptInt(s"Goals for $teamB: ")
        canPlay <- IO.pure(reader.canSchedule(teamA, teamB, current.currentRound, current.matches).run(cfg))
        st      <- if !canPlay then print(s"Cannot schedule $teamA vs $teamB.").map(_ => current)
        else processMatch(cfg, current, teamA, teamB, goalsA, goalsB)
      yield st

  private def processMatch(cfg: TournamentConfig, current: TournamentState,
                           teamA: String, teamB: String, goalsA: Int, goalsB: Int): IO[TournamentState] =
    val result = MatchResult(teamA, teamB, goalsA, goalsB, current.currentRound)
    val (st1, opt) = state.recordMatch(result).run(current)
    opt match
      case Some(m) => logMatchResult(cfg, current, st1, m, teamA, teamB)
      case None    => print("Failed to record match.").map(_ => current)

  private def logMatchResult(cfg: TournamentConfig, oldSt: TournamentState, newSt: TournamentState,
                             m: MatchResult, teamA: String, teamB: String): IO[TournamentState] =
    val oldTable = state.getTable.run(oldSt)._2
    val newTable = state.getTable.run(newSt)._2
    val outcomeA = TournamentLogic.outcomeFor(teamA, m)
    val outcomeB = TournamentLogic.outcomeFor(teamB, m)
    val ptsA = reader.pointsFor(outcomeA).run(cfg)
    val ptsB = reader.pointsFor(outcomeB).run(cfg)
    val oldPosA = TournamentLogic.positionOf(teamA, oldTable)
    val oldPosB = TournamentLogic.positionOf(teamB, oldTable)
    val newPosA = TournamentLogic.positionOf(teamA, newTable)
    val newPosB = TournamentLogic.positionOf(teamB, newTable)
    for
      _ <- logMatch(m)
      _ <- logPoints(teamA, outcomeA, ptsA)
      _ <- logPoints(teamB, outcomeB, ptsB)
      _ <- logPosition(teamA, oldPosA, newPosA)
      _ <- logPosition(teamB, oldPosB, newPosB)
      _ <- print("Match recorded.")
    yield newSt

  private def doShowTable(cfg: TournamentConfig, current: TournamentState): IO[TournamentState] =
    val sorted = reader.ranking(current.teams, current.matches).run(cfg)
    printTable(sorted).map(_ => current)

  private def doShowMatches(current: TournamentState): IO[TournamentState] =
    printMatches(current.matches).map(_ => current)

  private def doNextRound(current: TournamentState): IO[TournamentState] =
    val (st, round) = state.nextRound.run(current)
    print(s"Round $round started.").map(_ => st)

  private def doReset(current: TournamentState): IO[TournamentState] =
    val (st, _) = state.resetTournament.run(current)
    print("Tournament reset.").map(_ => st)

  private def doExit(current: TournamentState): IO[Unit] =
    print(s"Final: ${current.teams.size} teams, ${current.matches.size} matches. Goodbye!")

  // --- Цикл ---

  private def statusTitle(st: TournamentState): String =
    s"Tournament Round ${st.currentRound} | Teams: ${st.teams.size} | Matches: ${st.matches.size}"

  private def buildMenu(cfg: TournamentConfig, st: TournamentState): MenuTreeNode =
    MenuTreeNode(
      title = statusTitle(st),
      children = Seq(
        MenuLeaf("Add team", doAddTeam(st).flatMap(loop(cfg, _))),
        MenuLeaf("Record match result", doRecordMatch(cfg, st).flatMap(loop(cfg, _))),
        MenuLeaf("Show table", doShowTable(cfg, st).flatMap(loop(cfg, _))),
        MenuLeaf("Show matches", doShowMatches(st).flatMap(loop(cfg, _))),
        MenuLeaf("Next round", doNextRound(st).flatMap(loop(cfg, _))),
        MenuLeaf("Reset tournament", doReset(st).flatMap(loop(cfg, _))),
        MenuLeaf("Exit", doExit(st))
      )
    )

  private def loop(cfg: TournamentConfig, st: TournamentState): IO[Unit] =
    val menu = buildMenu(cfg, st)
    for
      _      <- printStatus(st)
      _      <- printMenuItems(menu)
      answer <- IO.getStrLn
      _      <- menu.handleUserAnswer(answer)
    yield ()

  private def printStatus(st: TournamentState): IO[Unit] =
    IO.putStrLn(s"\n=== ${statusTitle(st)} ===")

  private def printMenuItems(menu: MenuTreeNode): IO[Unit] =
    IO.delay(menu.children.zipWithIndex.foreach { (opt, i) => println(s"${i + 1}) ${opt.show}") })

  def handleUserAnswer(answer: String): IO[Unit] = IO.pure(())

  def userInteractionLoop: IO[Unit] =
    loop(TournamentConfig.default, TournamentState.empty)

  val program: IO[Unit] =
    for
      _ <- print("=== Tournament Table (variant 17) ===")
      _ <- userInteractionLoop
    yield ()

  def main(args: Array[String]): Unit =
    program.unsafeRun()