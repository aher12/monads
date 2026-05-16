package tournament

import monads.{Writer, Monoid}
import monads.Monoid.given

type Log = Vector[String]

object WriterOps:

  def interpreter: TournamentWriter[[A] =>> Writer[Log, A]] =
    new TournamentWriter[[A] =>> Writer[Log, A]]:
      def tell(msg: String): Writer[Log, Unit] =
        Writer.tell(Vector(msg))