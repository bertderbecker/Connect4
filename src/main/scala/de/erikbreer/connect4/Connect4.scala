package de.erikbreer.connect4


import javafx.application.{Application, Platform}
import javafx.scene.{Node, Parent => JFXParent}
import javafx.scene.layout.{Border, BorderStroke, BorderStrokeStyle, BorderWidths, CornerRadii, VBox => JFXVBox}
import javafx.scene.paint.Color
import io.github.bertderbecker.scalapfui.javafx.{FXElement, FXParent, JFXApp}
import io.github.bertderbecker.scalapfui.property.ReadableProperty

import scala.collection.immutable
import scala.language.{higherKinds, implicitConversions}

object Connect4 extends JFXApp {

  case class Player(index: Int, colors: Seq[Color], name: String) {
    override def toString: String = name
    def color: Color = colors(index)
  }

  case class Settings(height: Int, width: Int, kiLevel: Int, beginningPlayerIndex: Int, numberOfPlayers: Int)
  //Konstanten können ohne Probleme verändert werden !
  val defaultSettings =
    Settings(height = 6, width = 7, kiLevel = 5, beginningPlayerIndex =  0, numberOfPlayers = 2)

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

  def increaseIndex(index: Int): Int = if (index < (defaultSettings.numberOfPlayers - 1)) index + 1 else 0

  case class PlayerStone(player: Player, column: Int)
  case class StonesOfColumn(column: Int, stones: Seq[PlayerStone])

  def convertHistory(implicit game: Game, settings: Settings): Seq[StonesOfColumn] = {
    println("convertHistory " + game.history)
    val playerTupleSeq: Seq[(Int, Player)] =
      game.history zip
        Seq.iterate(
          game.players(settings.beginningPlayerIndex),
          game.history.size
        )(p => game.players(increaseIndex(p.index)))
    val stonesOfColumns =
      playerTupleSeq
        .map(tuple => PlayerStone(tuple._2, tuple._1))
        .groupBy(_.column)
        .map(tuple => StonesOfColumn(tuple._1, tuple._2.reverse))
        .toSeq
    val takenColumns = stonesOfColumns.map(_.column)
    val emptyColumns =
      for (i <- 1 to settings.width if !takenColumns.contains(i))
        yield StonesOfColumn(i, Seq.empty)
    (stonesOfColumns ++ emptyColumns).sortBy(_.column)
  }

  primaryStage = UI.pstage


}
