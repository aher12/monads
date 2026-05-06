package tournament

import monads.Reader

// Набор вычислений, зависящих от TournamentConfig.
// Каждая функция возвращает Reader — описание зависимости, а не готовый результат.
// Конфиг не передаётся явно внутри — он придёт снаружи при вызове .run(cfg).
object ReaderOps:

  // Сколько очков получает команда за конкретный исход.
  // outcome — Win, Draw или Loss.
  def pointsFor(outcome: Outcome): Reader[TournamentConfig, Int] =
    Reader.asks { cfg =>
      outcome match
        case Outcome.Win  => cfg.pointsForWin
        case Outcome.Draw => cfg.pointsForDraw
        case Outcome.Loss => cfg.pointsForLoss
    }

  // Строит отсортированную турнирную таблицу по списку команд и матчей.
  // Сначала считаем очки, затем сортируем согласно правилу тай-брейка из конфига.
  def ranking(
               teams:   List[String],
               matches: List[MatchResult]
             ): Reader[TournamentConfig, List[TeamRecord]] =
    Reader.asks { cfg =>
      // Для каждой команды собираем статистику по всем её матчам.
      val records = teams.map { team =>
        val teamMatches = matches.filter(m => m.teamA == team || m.teamB == team)
        var points        = 0
        var goalsScored   = 0
        var goalsConceded = 0
        var wins          = 0
        var draws         = 0
        var losses        = 0

        teamMatches.foreach { m =>
          val (scored, conceded) =
            if m.teamA == team then (m.goalsA, m.goalsB)
            else (m.goalsB, m.goalsA)

          goalsScored   += scored
          goalsConceded += conceded

          if scored > conceded then
            wins   += 1
            points += cfg.pointsForWin
          else if scored == conceded then
            draws  += 1
            points += cfg.pointsForDraw
          else
            losses += 1
            points += cfg.pointsForLoss
        }

        TeamRecord(team, points, goalsScored, goalsConceded, wins, draws, losses)
      }

      // Сортировка по правилу тай-брейка.
      cfg.tieBreak match
        case TieBreakRule.GoalDifference =>
          records.sortBy(r => -r.points).sortBy(r => -(r.goalsScored - r.goalsConceded))
        case TieBreakRule.GoalsScored =>
          records.sortBy(r => -r.points).sortBy(r => -r.goalsScored)
        case TieBreakRule.HeadToHead =>
          // Упрощённо: сортируем по очкам, head-to-head не реализуем полноценно.
          records.sortBy(r => -r.points)
        case TieBreakRule.GoalDiffThenScored =>
          records.sortBy(r => (-r.points, -(r.goalsScored - r.goalsConceded), -r.goalsScored))
    }

  // Дополнительная проверка с учётом уже сыгранных матчей.
  // Эту логику используем снаружи, передавая state.matches.
  def canScheduleWithHistory(
                              teamA:   String,
                              teamB:   String,
                              round:   Int,
                              matches: List[MatchResult]
                            ): Reader[TournamentConfig, Boolean] =
    Reader.asks { cfg =>
      if teamA == teamB then false
      else if !cfg.forbidRematch then true
      else
        val alreadyPlayed = matches.exists { m =>
          m.round == round &&
            ((m.teamA == teamA && m.teamB == teamB) || (m.teamA == teamB && m.teamB == teamA))
        }
        !alreadyPlayed
    }

  // Сравнивает две команды по тай-брейку.
  // Возвращает отрицательное, если teamA выше, положительное, если teamB выше.
  def tieBreak(
                recordA: TeamRecord,
                recordB: TeamRecord
              ): Reader[TournamentConfig, Int] =
    Reader.asks { cfg =>
      if recordA.points != recordB.points then
        recordB.points - recordA.points // больше очков — выше
      else
        cfg.tieBreak match
          case TieBreakRule.GoalDifference =>
            val diffA = recordA.goalsScored - recordA.goalsConceded
            val diffB = recordB.goalsScored - recordB.goalsConceded
            diffB - diffA
          case TieBreakRule.GoalsScored =>
            recordB.goalsScored - recordA.goalsScored
          case TieBreakRule.HeadToHead =>
            0 // упрощённо
          case TieBreakRule.GoalDiffThenScored =>
            val diffA = recordA.goalsScored - recordA.goalsConceded
            val diffB = recordB.goalsScored - recordB.goalsConceded
            if diffA != diffB then diffB - diffA
            else recordB.goalsScored - recordA.goalsScored
    }