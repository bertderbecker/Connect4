package de.erikbreer.connect4

import javafx.scene.Node
import javafx.scene.layout.{Border, BorderStroke, BorderStrokeStyle, BorderWidths, CornerRadii, VBox => JFXVBox}
import javafx.scene.paint.Color
import javafx.scene.text.Font

import cats.implicits._
import de.erikbreer.connect4.Connect4.{defaultGame, defaultSettings}
import io.github.bertderbecker.scalapfui.attribute.StoredReadableAttribute
import io.github.bertderbecker.scalapfui.javafx.Implicits._
import io.github.bertderbecker.scalapfui.javafx.attribute.FXStoredReadableAttribute
import io.github.bertderbecker.scalapfui.javafx.event.EventReactor.rebuild
import io.github.bertderbecker.scalapfui.javafx.property.Conditions.when
import io.github.bertderbecker.scalapfui.javafx.scene.SceneExts.Scene
import io.github.bertderbecker.scalapfui.javafx.scene.control.LabelExts._
import io.github.bertderbecker.scalapfui.javafx.scene.control.MenuBarExts.{MenuBar, menuBar}
import io.github.bertderbecker.scalapfui.javafx.scene.control.MenuExts.{Menu, menu}
import io.github.bertderbecker.scalapfui.javafx.scene.control.MenuItemExts.{MenuItem, menuItem}
import io.github.bertderbecker.scalapfui.javafx.scene.layout.OrderedBoxes.{HBox, VBox, hBox, vBox}
import io.github.bertderbecker.scalapfui.javafx.scene.shape.RectangleExts.{Rectangle, rectangle}
import io.github.bertderbecker.scalapfui.javafx.stage.StageExts.{Stage, stage}
import io.github.bertderbecker.scalapfui.javafx.{FXElement, FXParent}

object UI {

  val stageWidth: StoredReadableAttribute[Double] = FXStoredReadableAttribute[Double]()
  val stageHeight: StoredReadableAttribute[Double] = FXStoredReadableAttribute[Double]()

  val defaultStageWidth = 800 //Konstanten können ohne Probleme verändert werden !
  val defaultStageHeight = 400 //Konstanten können ohne Probleme verändert werden !

  val menuBarHeight = 30

  def pstage =
    Stage(
      stage.title := "Connect 4",
      stage.initWidth := defaultStageWidth,
      stage.initHeight := defaultStageHeight,
      stage.width ==> stageWidth,
      stage.height ==> stageHeight,
      stage.scene := Scene(
        UI.layout(defaultGame, defaultSettings)
      )()
    )


  def layout(implicit game: Connect4.Game, settings: Connect4.Settings): FXParent[JFXVBox] = {
    VBox(
      HBox(
        MenuBar(
          Menu(
            MenuItem(
              menuItem.text := "Neues Spiel",
              menuItem.onAction := rebuild { _ =>
                layout(
                  game.copy(
                    history = Seq.empty
                  ),
                  settings
                )
              }
            ),
            MenuItem(
              menuItem.text := "Farben neu festlegen",
              menuItem.onAction := rebuild { _ =>
                layout(
                  Connect4.reloadColors,
                  settings
                )
              }
            ),
            MenuItem(
              menuItem.text := "Layout zum Debuggen neu laden",
              menuItem.onAction := rebuild { _ => layout }
            )
          )(
            menu.text := "Spiel"
          )
        )(
          menuBar.prefHeight := menuBarHeight,
          menuBar.prefWidth <== stageWidth * 0.5
        ),
        Label(
          label.text := "Aktueller Spieler: " + game.nextPlayer,
          label.textFill := Color.RED,
          label.font <== stageWidth.map(v => Font.font(v / 40))
        )
      )(
        hBox.prefHeight := menuBarHeight,
        hBox.prefWidth <== stageWidth,
        hBox.backgroundFill := Color.BLACK
      ),
      HBox(
        stones: _*
      )(
        hBox.layoutY := 30,
        hBox.layoutX := 0,
        hBox.prefWidth <== stageWidth,
        hBox.prefHeight <== stageHeight - menuBarHeight
      )
    )()
  }

  def stones(implicit game: Connect4.Game, settings: Connect4.Settings): Seq[FXElement[_ <: Node]] = {
    Connect4.convertHistory
      .foldLeft(Seq.empty[FXElement[_ <: Node]]) { (columnSeq, columnPlayerStones) =>
        columnSeq :+
          VBox(
            columnPlayerStones.stones.foldLeft(Seq.empty[FXElement[_ <: Node]]) { (stonesSeq, player) =>
              if (stonesSeq.lengthCompare(Connect4.defaultSettings.height) >= 0)
                throw new IllegalStateException("Ungültiger Zug !!!")
              stonesSeq :+ Rectangle(
                rectangle.height <== (stageHeight - 70) / settings.height,
                rectangle.width <== stageWidth / settings.width,
                rectangle.arcHeight := 40,
                rectangle.arcWidth := 40,
                rectangle.fill := player.color
              )
            }: _*
          )(
            vBox.prefWidth <== stageWidth / settings.width,
            vBox.prefHeight <== stageHeight - menuBarHeight,
            vBox.backgroundFill <== when(vBox.hover) choose Color.GRAY otherwise Color.BLACK,
            vBox.onMouseReleased := rebuild { _ =>
              val newGame = game.copy(
                history = game.history :+ columnPlayerStones.column
              )
              UI.layout(
                newGame,
                settings
              )
            },
            vBox.Alignment.BottomCenter,
            vBox.border := new Border(new BorderStroke(Color.WHITE, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, BorderWidths.DEFAULT))
          )
      }
  }
}
