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

            val empty = TeamRecord(team, 0, 0, 0, 0, 0, 0)

            val updates: List[TeamRecord => TeamRecord] = teamMatches.map { m =>
              val (s, c) =
                if m.teamA == team then (m.goalsA, m.goalsB)
                else (m.goalsB, m.goalsA)

              val outcome =
                if s > c then Outcome.Win
                else if s == c then Outcome.Draw
                else Outcome.Loss

              val earned = outcome match
                case Outcome.Win  => 3
                case Outcome.Draw => 1
                case Outcome.Loss => 0

              (r: TeamRecord) =>
                val (w1, d1, l1) = outcome match
                  case Outcome.Win  => (r.wins + 1, r.draws, r.losses)
                  case Outcome.Draw => (r.wins, r.draws + 1, r.losses)
                  case Outcome.Loss => (r.wins, r.draws, r.losses + 1)

                TeamRecord(
                  name          = r.name,
                  points        = r.points + earned,
                  goalsScored   = r.goalsScored + s,
                  goalsConceded = r.goalsConceded + c,
                  wins          = w1,
                  draws         = d1,
                  losses        = l1
                )
            }

            if updates.isEmpty then empty
            else updates.reduce((f1, f2) => f1.andThen(f2))(empty)
          }.sortBy(r => -r.points)
        }