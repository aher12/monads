package tournament

import monads.IO
import monads.IO.given

// Абстракция пункта меню — знает только как себя показать.
trait MenuOption:
  def show: String

// Абстракция взаимодействия с пользователем через IO.
// handleUserAnswer — обработать ввод и вернуть следующее состояние.
// userInteractionLoop — полный цикл: показать меню, прочитать ввод, обработать, повторить.
trait UserInteraction:
  def handleUserAnswer(answer: String): IO[Unit]
  def userInteractionLoop: IO[Unit]

// Листовой пункт меню — метка, действие и флаг, продолжать ли цикл.
// action — IO[Unit], описывает что произойдёт, не выполняет сразу.
// continue = true  -> после действия показать меню снова.
// continue = false -> выполнить действие и выйти (не возвращаться в цикл).
final case class MenuLeaf(
                           title:    String,
                           action:   IO[Unit],
                           continue: Boolean = true
                         ) extends MenuOption:
  def show: String = title

// Узел дерева меню — заголовок и список дочерних пунктов (листья или другие узлы).
// extends MenuOption with UserInteraction: сам является и пунктом, и обработчиком.
// children нумеруются автоматически — никакого хардкода "1", "2", "3".
final case class MenuTreeNode(
                               title:    String,
                               children: Seq[MenuOption]
                             ) extends MenuOption with UserInteraction:

  def show: String = title

  // Выводит пронумерованное меню. zipWithIndex — (элемент, индекс), +1 чтобы начинать с 1.
  private def printMenu: IO[Unit] =
    val lines = children.zipWithIndex.map { (opt, i) => s"${i + 1}) ${opt.show}" }
    val body  = lines.mkString("\n")
    IO.putStrLn(s"\n=== $title ===\n$body")

  // Обрабатывает строку ввода пользователя.
  // Число в диапазоне — выбрать дочерний пункт и выполнить его или войти в подменю.
  // При continue = false на MenuLeaf возвращает IO.pure(()) и цикл останавливается.
  // Всё остальное — ошибка, вернуться в тот же цикл.
  def handleUserAnswer(answer: String): IO[Unit] =
    answer.trim match
      case s =>
        s.toIntOption match
          case Some(n) if n >= 1 && n <= children.size =>
            children(n - 1) match
              case leaf: MenuLeaf =>
                if leaf.continue then
                  leaf.action.flatMap(_ => userInteractionLoop)
                else
                  leaf.action
              case node: MenuTreeNode =>
                node.userInteractionLoop.flatMap(_ => userInteractionLoop)
              case other =>
                IO.putStrLn(s"Unhandled menu option: ${other.show}")
                  .flatMap(_ => userInteractionLoop)
          case _ =>
            IO.putStrLn(s"Unknown command: '$s'.")
              .flatMap(_ => userInteractionLoop)

  // Полный цикл: показать меню → прочитать ввод → обработать
  // (handleUserAnswer рекурсивно продолжает или выходит).
  def userInteractionLoop: IO[Unit] =
    for
      _      <- printMenu
      answer <- IO.getStrLn
      _      <- handleUserAnswer(answer)
    yield ()