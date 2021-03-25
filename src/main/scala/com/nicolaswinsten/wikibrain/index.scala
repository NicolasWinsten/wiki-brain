package com.nicolaswinsten.wikibrain

import org.scalajs.dom
import org.scalajs.dom.{document}
import org.scalajs.dom.html.{Button, Input, UList}
import org.scalajs.dom.raw.{KeyboardEvent}
import scalatags.JsDom.all._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// todo text box shake when wrong answer and red notif
object index {

  var currentPage: Page = Page("", "", Set.empty, None)
  var wordBox: Set[String] = Set.empty
  var previousGuesses: Set[String] = Set.empty

  // user's input field
  val guess: Input = input(`type`:="text").render
  guess.onkeypress = (e: KeyboardEvent) => if (e.key == "Enter") {
    makeGuess(guess.value)
    guess.value = ""
  }

  // the reroll button
  val rerollBtn: Button = button(id:="reroll", "Reroll!").render
  rerollBtn.onclick = (_: dom.MouseEvent) => reroll()

  val correctGuesses: UList = ul(id:="correct-guesses").render

  def main(args: Array[String]): Unit = {
    document.body.appendChild(guess)
    document.body.appendChild(rerollBtn)
    document.body.appendChild(div(correctGuesses).render)
  }


  def reroll(): Unit = {
    println("rerolling...")
    Scraper.getRandomPage.onComplete({
      case Success(newPage) =>
        if (currentPage.title == newPage.title) reroll()
        else { currentPage = newPage; updatePageDisplay() }
      case Failure(exception) =>
        throw exception
    })
  }

  def updatePageDisplay(): Unit = {
    val display = document.getElementById("page-display")
    display.innerHTML = ""
    correctGuesses.innerHTML = ""

    display.appendChild( h1(currentPage.title).render )

    currentPage.image.foreach( file => display.appendChild( img(src:=file).render))

    display.appendChild( h3(currentPage.desc).render )
    wordBox = currentPage.items
    previousGuesses = Set.empty
  }

  def makeGuess(guessStr: String): Unit = {
    val guess = guessStr.toLowerCase
    if (previousGuesses contains guess) {
      println("user already guessed " + guess)
    } else if (wordBox contains guess) {
      println("user correctly guessed " + guess)
      correctGuesses.appendChild( li(guess).render )
      previousGuesses += guess
    } else println("user incorrectly guessed " + guess)
  }

}


