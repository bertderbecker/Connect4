package de.erikbreer.connect4

import javafx.scene.paint.Color

import io.github.bertderbecker.collection.extras.SeqRemover._
import io.github.bertderbecker.collection.extras.SeqShuffler._

object Colors {

  def allColors: Seq[Color] =
    for (field <- classOf[Color].getFields; color = field.get(); if color.isInstanceOf[Color])
      yield color.asInstanceOf[Color]

  val backgroundColor: Color = Color.BLACK
  val winningColor: Color = Color.BLUE

  val invalidColors: Seq[Color] =
    Seq(Color.RED, Color.DARKGREY, Color.BLUE, Color.MEDIUMBLUE, Color.TRANSPARENT)
  val suitableColors: Seq[Color] = allColors - backgroundColor - winningColor -- invalidColors

  def colors(number: Int): Seq[Color] =
    (1 to number)
      .foldLeft(Seq.empty[Color])((seq, i) => seq :+ (suitableColors -- seq).shuffled.randomElement)

}
