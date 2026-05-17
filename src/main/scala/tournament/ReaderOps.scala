package tournament

import monads.Reader

object ReaderOps:

  def interpreter: TournamentReader[[A] =>> Reader[TournamentConfig, A]] =
    new TournamentReader[[A] =>> Reader[TournamentConfig, A]]:

      def pointsFor(outcome: Outcome): Reader[TournamentConfig, Int] =
        Reader.asks { cfg =>
          outcome match
            case Outcome.Win  => cfg.pointsForWin
            case Outcome.Draw => cfg.pointsForDraw
            case Outcome.Loss => cfg.pointsForLoss
        }

      def ranking(teams: List[String], matches: List[MatchResult]): Reader[TournamentConfig, List[TeamRecord]] =
        Reader.asks { cfg =>
          val standings = TournamentLogic.computeStandings(teams, matches, cfg.pointsForWin, cfg.pointsForDraw, cfg.pointsForLoss)
          TournamentLogic.sortWithTieBreak(standings, (a, b) => tieBreak(a, b).run(cfg))
        }

      def canSchedule(
                       teamA: String, teamB: String, round: Int, matches: List[MatchResult]
                     ): Reader[TournamentConfig, Boolean] =
        Reader.asks { cfg =>
          if teamA == teamB then false
          else if !cfg.forbidRematch then true
          else
            !matches.exists { m =>
              m.round == round &&
                ((m.teamA == teamA && m.teamB == teamB) || (m.teamA == teamB && m.teamB == teamA))
            }
        }

      def tieBreak(recordA: TeamRecord, recordB: TeamRecord): Reader[TournamentConfig, Int] =
        Reader.asks { cfg =>
          if recordA.points != recordB.points then recordB.points - recordA.points
          else
            cfg.tieBreak match
              case TieBreakRule.GoalDifference =>
                (recordB.goalsScored - recordB.goalsConceded) - (recordA.goalsScored - recordA.goalsConceded)
              case TieBreakRule.GoalsScored =>
                recordB.goalsScored - recordA.goalsScored
              case TieBreakRule.HeadToHead => 0
              case TieBreakRule.GoalDiffThenScored =>
                val diffA = recordA.goalsScored - recordA.goalsConceded
                val diffB = recordB.goalsScored - recordB.goalsConceded
                if diffA != diffB then diffB - diffA
                else recordB.goalsScored - recordA.goalsScored
        }