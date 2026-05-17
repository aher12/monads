package tournament

import monads.IO
import monads.IO.given

trait MenuOption:
  def show: String

trait UserInteraction:
  def handleUserAnswer(answer: String): IO[Unit]
  def userInteractionLoop: IO[Unit]

final case class MenuLeaf(
                           title:    String,
                           action:   IO[Unit],
                           continue: Boolean = true
                         ) extends MenuOption:
  def show: String = title

final case class MenuTreeNode(
                               title:    String,
                               children: Seq[MenuOption]
                             ) extends MenuOption with UserInteraction:

  def show: String = title

  private def printMenu: IO[Unit] =
    val lines = children.zipWithIndex.map { (opt, i) => s"${i + 1}) ${opt.show}" }
    IO.putStrLn(s"\n=== $title ===\n${lines.mkString("\n")}")

  def handleUserAnswer(answer: String): IO[Unit] =
    answer.trim match
      case s =>
        s.toIntOption match
          case Some(n) if n >= 1 && n <= children.size =>
            children(n - 1) match
              case leaf: MenuLeaf  => leaf.action
              case node: MenuTreeNode => node.userInteractionLoop.flatMap(_ => userInteractionLoop)
              case other => IO.putStrLn(s"Unhandled: ${other.show}").flatMap(_ => userInteractionLoop)
          case _ => IO.putStrLn(s"Unknown command: '$s'.").flatMap(_ => userInteractionLoop)

  def userInteractionLoop: IO[Unit] =
    for
      _      <- printMenu
      answer <- IO.getStrLn
      _      <- handleUserAnswer(answer)
    yield ()