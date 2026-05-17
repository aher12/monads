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
          TournamentLogic.computeStandings(st.teams, st.matches, 3, 1, 0)
        }