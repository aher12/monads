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
          val records = teams.map { team =>
            val teamMatches = matches.filter(m => m.teamA == team || m.teamB == team)

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
                case Outcome.Win  => cfg.pointsForWin
                case Outcome.Draw => cfg.pointsForDraw
                case Outcome.Loss => cfg.pointsForLoss

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
          }

          records.sortWith { (a, b) =>
            val cmp = tieBreak(a, b).run(cfg)
            cmp < 0
          }
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