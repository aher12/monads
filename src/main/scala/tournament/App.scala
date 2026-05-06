package tournament

import monads.{IO, Writer, State, Monad}
import monads.Monoid.given
import monads.IO.given

// UserInteraction — весь main теперь живёт здесь как дерево MenuTreeNode.
// Никаких case "1" / case "2" — меню масштабируется добавлением узлов в дерево.
object App extends UserInteraction:

  private val readLine: IO[String]       = IO.getStrLn
  private def print(s: String): IO[Unit] = IO.putStrLn(s)

  private def prompt(msg: String): IO[String] =
    for
      _    <- print(msg)
      line <- readLine
    yield line

  private def promptInt(msg: String): IO[Int] =
    prompt(msg).map(_.toInt)

  // Вывод турнирной таблицы с использованием Reader для сортировки.
  private def printTable(
                          records: List[TeamRecord],
                          cfg:     TournamentConfig
                        ): IO[Unit] =
    val header = "| Pos | Team           | Pts | W | D | L | GF | GA | GD |"
    val separator = "|-----|----------------|-----|---|---|---|---|----|----|----|"
    val lines = records
      .sortBy(r => (-r.points, -(r.goalsScored - r.goalsConceded), -r.goalsScored))
      .zipWithIndex
      .map { (r, i) =>
        f"| ${i + 1}%3d | ${r.name}%-14s | ${r.points}%3d | ${r.wins}%1d | ${r.draws}%1d | ${r.losses}%1d | ${r.goalsScored}%2d | ${r.goalsConceded}%2d | ${r.goalDifference}%3d |"
      }
    val table = (header :: separator :: lines).mkString("\n")
    print(s"\n$table\n")

  // Вывод результатов матчей.
  private def printMatches(matches: List[MatchResult]): IO[Unit] =
    if matches.isEmpty then print("No matches played yet.")
    else
      val lines = matches.map { m =>
        s"Round ${m.round}: ${m.teamA} ${m.goalsA} - ${m.goalsB} ${m.teamB}"
      }
      print(lines.mkString("\n"))

  // --- Потоки операций ---

  // Добавление команды.
  private def addTeamFlow(stRef: StRef): IO[Unit] =
    for
      name   <- prompt("Team name: ")
      (st1, added) = StateOps.addTeam(name).run(stRef.get)
      _      <- if added
      then print(s"Team '$name' added.").map(_ => stRef.set(st1))
      else print(s"Team '$name' already exists.").map(_ => stRef.set(st1))
    yield ()

  // Добавление результата матча.
  private def recordMatchFlow(cfg: TournamentConfig, stRef: StRef): IO[Unit] =
    val st = stRef.get
    if st.teams.size < 2 then
      print("Need at least 2 teams to record a match.")
    else
      for
        _       <- print(s"Teams: ${st.teams.mkString(", ")}")
        teamA   <- prompt("Team A: ")
        teamB   <- prompt("Team B: ")
        goalsA  <- promptInt(s"Goals for $teamA: ")
        goalsB  <- promptInt(s"Goals for $teamB: ")
        round    = st.currentRound
        canPlay  = ReaderOps.canScheduleWithHistory(teamA, teamB, round, st.matches).run(cfg)
        _       <- if !canPlay then
          print(s"Cannot schedule $teamA vs $teamB in round $round (already played or same team).")
        else
          val result = MatchResult(teamA, teamB, goalsA, goalsB, round)
          val (st1, optResult) = StateOps.recordMatch(result).run(st)
          optResult match
            case Some(m) =>
              val Writer(logEntries, _) = WriterOps.logMatchResult(m)
              for
                _ <- print(s"Match recorded: ${m.teamA} ${m.goalsA} - ${m.goalsB} ${m.teamB}")
                _ <- IO.delay { logEntries.foreach(println) }
                _ <- IO.delay { stRef.set(st1) }
              yield ()
            case None =>
              print("Failed to record match (teams not found or already played this round).")
      yield ()

  // Вывод турнирной таблицы.
  private def showTableFlow(cfg: TournamentConfig, stRef: StRef): IO[Unit] =
    val st      = stRef.get
    val records = StateOps.getTable.run(st)._2 // значение из (state, value)
    records match
      case Nil => print("No teams in tournament.")
      case _   => printTable(records, cfg)

  // Вывод сыгранных матчей.
  private def showMatchesFlow(stRef: StRef): IO[Unit] =
    val st = stRef.get
    printMatches(st.matches)

  // Переход к следующему туру.
  private def nextRoundFlow(stRef: StRef): IO[Unit] =
    val (st1, round) = StateOps.nextRound.run(stRef.get)
    print(s"Round $round started.").map(_ => stRef.set(st1))

  // Сброс турнира.
  private def resetFlow(stRef: StRef): IO[Unit] =
    val (st1, oldSt) = StateOps.resetTournament.run(stRef.get)
    print(s"Tournament reset. Old state: ${oldSt.teams.size} teams, ${oldSt.matches.size} matches.")
      .map(_ => stRef.set(st1))

  // Выход с финальным отчётом.
  private def exitFlow(stRef: StRef): IO[Unit] =
    val st = stRef.get
    for
      _ <- print(s"Final: ${st.teams.size} teams, ${st.matches.size} matches in ${st.currentRound - 1} rounds.")
      _ <- print("Goodbye!")
    yield ()

  // Изменяемая ссылка на состояние — обёртка для передачи в MenuLeaf-замыкания.
  // Scala — функциональный язык, но для дерева меню нам нужен общий изменяемый стейт,
  // иначе каждый MenuLeaf получит копию состояния на момент построения дерева.
  private final class StRef(private var state: TournamentState):
    def get: TournamentState      = state
    def set(s: TournamentState): Unit = state = s

  // Строит заголовок статус-бара для меню.
  private def statusTitle(stRef: StRef): String =
    val st = stRef.get
    s"Tournament Round ${st.currentRound} | Teams: ${st.teams.size} | Matches: ${st.matches.size}"

  // Дерево меню строится один раз. Добавить новый пункт = добавить MenuLeaf.
  // Нет ни одного case "N" — нумерация автоматическая в MenuTreeNode.
  private def buildMenu(cfg: TournamentConfig, stRef: StRef): MenuTreeNode =
    MenuTreeNode(
      title = statusTitle(stRef),
      children = Seq(
        MenuLeaf("Add team",              addTeamFlow(stRef)),
        MenuLeaf("Record match result",   recordMatchFlow(cfg, stRef)),
        MenuLeaf("Show table",            showTableFlow(cfg, stRef)),
        MenuLeaf("Show matches",          showMatchesFlow(stRef)),
        MenuLeaf("Next round",            nextRoundFlow(stRef)),
        MenuLeaf("Reset tournament",      resetFlow(stRef)),
        MenuLeaf("Exit",                  exitFlow(stRef), continue = false)
      )
    )

  // handleUserAnswer и userInteractionLoop делегируют в buildMenu.
  // App сам является UserInteraction — как и требует доска.
  def handleUserAnswer(answer: String): IO[Unit] =
    val stRef = StRef(TournamentState.empty)
    buildMenu(TournamentConfig.default, stRef).handleUserAnswer(answer)

  def userInteractionLoop: IO[Unit] =
    val stRef = StRef(TournamentState.empty)
    val cfg   = TournamentConfig.default
    // Цикл с обновляемым заголовком: каждую итерацию пересобираем заголовок из stRef.
    def loop: IO[Unit] =
      val menu = buildMenu(cfg, stRef)
      for
        _      <- IO.putStrLn(s"\n=== ${statusTitle(stRef)} ===")
        _      <- IO.delay {
          menu.children.zipWithIndex.foreach { (opt, i) =>
            println(s"${i + 1}) ${opt.show}")
          }
        }
        answer <- IO.getStrLn
        _      <- answer.trim match
          case s =>
            s.toIntOption match
              case Some(n) if n >= 1 && n <= menu.children.size =>
                menu.children(n - 1) match
                  case leaf: MenuLeaf =>
                    if leaf.continue then
                      leaf.action.flatMap(_ => loop)
                    else
                      leaf.action
                  case node: MenuTreeNode =>
                    node.userInteractionLoop.flatMap(_ => loop)
                  case other =>
                    IO.putStrLn(s"Unhandled: ${other.show}").flatMap(_ => loop)
              case _ =>
                IO.putStrLn(s"Unknown command: '$s'.").flatMap(_ => loop)
      yield ()
    loop

  val program: IO[Unit] =
    for
      _ <- print("=== Tournament Table (variant 17) ===")
      _ <- userInteractionLoop
    yield ()

  def main(args: Array[String]): Unit =
    program.unsafeRun()