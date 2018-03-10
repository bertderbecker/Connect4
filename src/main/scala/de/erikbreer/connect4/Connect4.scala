package de.erikbreer.connect4


import javafx.scene.layout.{VBox => JFXVBox}
import javafx.scene.paint.Color
import javafx.scene.{Parent => JFXParent}

import io.github.bertderbecker.scalapfui.javafx.JFXApp

import scala.language.{higherKinds, implicitConversions}

object Connect4 extends JFXApp {

  case class Player(index: Int, colors: Seq[Color], name: String) {
    override def toString: String = name
    def color: Color = colors(index)
  }

  case class Settings(height: Int, width: Int, beginningPlayerIndex: Int, numberOfPlayers: Int)
  //Konstanten können ohne Probleme verändert werden !
  val defaultSettings =
    Settings(height = 6, width = 7, beginningPlayerIndex = 0, numberOfPlayers = 3)

  def calcColors(): Seq[Color] = Colors.colors(defaultSettings.numberOfPlayers)
  private val colors = calcColors()

  case class Game(history: Seq[Int], players: Seq[Player]) {
    lazy val nextPlayer = players.apply(history.size % players.size)
  }

  //Diese Einstellungen betreffen die Logik des Spiels und sollten nicht veändert werden !
  val defaultGame =
    Game(
      history = Seq.empty,
      players =
        for (n <- 0 until defaultSettings.numberOfPlayers)
          yield Player(n, colors, "Spieler " + n.toString)
    )


  def reloadColors(implicit game: Game): Game = {
    val newColors = calcColors()
    game.copy(players = game.players.map(_.copy(colors = newColors)))
  }

  case class StonesOfColumn(column: Int, stones: Seq[Player])

  def convertHistory(implicit game: Game, settings: Settings): Seq[StonesOfColumn] = {
    println("convertHistory " + game.history)
    val stonesOfColumns: Seq[StonesOfColumn] =
      (game.history zip
        Seq.iterate(
          game.players(settings.beginningPlayerIndex),
          game.history.size
        )(p => game.players((p.index + 1) % game.players.size)))
        .groupBy(_._1)
        .map(tuple => StonesOfColumn(tuple._1, tuple._2.reverse.map(_._2)))
        .toSeq
    val emptyColumns =
      for (i <- 1 to settings.width if !stonesOfColumns.map(_.column).contains(i))
        yield StonesOfColumn(i, Seq.empty)
    (stonesOfColumns ++ emptyColumns).sortBy(_.column)
  }

  primaryStage = UI.pstage


}
