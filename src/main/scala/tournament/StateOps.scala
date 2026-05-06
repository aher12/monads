package tournament

import monads.State

object StateOps:

  // Добавляет команду в турнир, если её ещё нет.
  // Возвращает true если команда добавлена, false если уже существует.
  def addTeam(name: String): State[TournamentState, Boolean] =
    State { st =>
      if st.teams.contains(name) then
        (st, false)
      else
        (st.copy(teams = st.teams :+ name), true)
    }

  // Записывает результат матча в историю.
  // Проверяет, что обе команды существуют и матч ещё не сыгран в этом туре.
  def recordMatch(result: MatchResult): State[TournamentState, Option[MatchResult]] =
    State { st =>
      val teamExists = st.teams.contains(result.teamA) && st.teams.contains(result.teamB)
      val alreadyPlayed = st.matches.exists { m =>
        m.round == result.round &&
          ((m.teamA == result.teamA && m.teamB == result.teamB) ||
            (m.teamA == result.teamB && m.teamB == result.teamA))
      }
      if teamExists && !alreadyPlayed then
        (st.copy(matches = st.matches :+ result), Some(result))
      else
        (st, None)
    }

  // Переходит к следующему туру (currentRound + 1).
  // Возвращает номер нового тура.
  val nextRound: State[TournamentState, Int] =
    State { st =>
      val newRound = st.currentRound + 1
      (st.copy(currentRound = newRound), newRound)
    }

  // Сбрасывает турнир до начального состояния.
  // Возвращает старое состояние для отчёта.
  val resetTournament: State[TournamentState, TournamentState] =
    State { st =>
      (TournamentState.empty, st)
    }

  // Получить текущую таблицу (рассчитывается из teams и matches).
  // Возвращает список TeamRecord, отсортированный по очкам.
  // Сама сортировка по тай-брейку делается через ReaderOps.ranking.
  def getTable: State[TournamentState, List[TeamRecord]] =
    State.inspect { st =>
      // Без конфига не можем отсортировать по тай-брейку — сортируем только по очкам.
      val records = st.teams.map { team =>
        val teamMatches = st.matches.filter(m => m.teamA == team || m.teamB == team)
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
            points += 3 // hardcoded, будет пересчитано в ranking
          else if scored == conceded then
            draws  += 1
            points += 1
          else
            losses += 1
        }
        TeamRecord(team, points, goalsScored, goalsConceded, wins, draws, losses)
      }
      records.sortBy(r => -r.points)
    }

  // Краткая информация о состоянии для заголовка меню.
  def summary: State[TournamentState, String] =
    State.inspect { st =>
      s"Round ${st.currentRound} | Teams: ${st.teams.size} | Matches: ${st.matches.size}"
    }