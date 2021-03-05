package com.nicolaswinsten.wikibrain

import org.scalajs.dom
import org.scalajs.dom.document
import scalatags.JsDom.all._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}


object index {

  var wordBox: Set[String] = Set.empty

  def main(args: Array[String]): Unit = {
    val reroll = button(id:="reroll", "Reroll!").render
    println(reroll)
    reroll.addEventListener("click", (e: dom.MouseEvent) => {
      Scraper.getRandomPage.onComplete({
        case Success(page) => updatePageDisplay(page)
        case Failure(exception) => throw exception
      })
    })

    document.body.appendChild(reroll)
  }

  def updatePageDisplay(page: Page): Unit = {
    val display = document.getElementById("page-display")

    display.appendChild( h1(page.title).render )
    if (page.image.isDefined) display.appendChild( img(src:=page.image.get).render)
    display.appendChild( h3(page.desc).render )
    wordBox = page.items
  }

}