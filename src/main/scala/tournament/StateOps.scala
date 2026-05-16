package tournament

import monads.State

object StateOps:

  def interpreter: TournamentStateOps[[A] =>> State[TournamentState, A]] =
    new TournamentStateOps[[A] =>> State[TournamentState, A]]:

      def addTeam(name: String): State[TournamentState, Boolean] =
        State { st =>
          if st.teams.contains(name) then (st, false)
          else (st.copy(teams = st.teams :+ name), true)
        }

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

      def nextRound: State[TournamentState, Int] =
        State { st =>
          val newRound = st.currentRound + 1
          (st.copy(currentRound = newRound), newRound)
        }

      def resetTournament: State[TournamentState, TournamentState] =
        State { st => (TournamentState.empty, st) }

      def getTable: State[TournamentState, List[TeamRecord]] =
        State.inspect { st =>
          st.teams.map { team =>
            val teamMatches = st.matches.filter(m => m.teamA == team || m.teamB == team)

            val init = (0, 0, 0, 0, 0, 0)
            val (points, scored, conceded, wins, draws, losses) =
              teamMatches.foldLeft(init) { case ((pts, sc, conc, w, d, l), m) =>
                val (s, c) =
                  if m.teamA == team then (m.goalsA, m.goalsB)
                  else (m.goalsB, m.goalsA)

                if s > c then (pts + 3, sc + s, conc + c, w + 1, d, l)
                else if s == c then (pts + 1, sc + s, conc + c, w, d + 1, l)
                else (pts, sc + s, conc + c, w, d, l + 1)
              }

            TeamRecord(team, points, scored, conceded, wins, draws, losses)
          }.sortBy(r => -r.points)
        }