package com.nicolaswinsten.wikibrain

import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.XMLHttpRequest

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scalajs.js.URIUtils.{decodeURI, encodeURI}

/**
 * web scraping object to work on wikipedia
 *
 * WARNING: I use regex to parse the HTML. Hopefully, I'll learn a good jQuery facade at some point
 */
object Scraper {
  val url = "https://en.wikipedia.org/wiki/"

  def fetchHTML(url: String): Future[String] = {
//    import fr.hmil.roshttp.HttpRequest
//    import monix.execution.Scheduler.Implicits.global

    // TODO try ajax here instead and use the MediaWiki api :(
    println("fetching " + url)
    val e = encodeURI(url)
    println(e)
//    HttpRequest(e).withHeader("Access-Control-Allow-Origin", "*").get() map (_.body)
    val requestHeaders = Map(
      "Origin" -> "*",
      "Content-Type" -> "application/json; charset=UTF-8")
    Ajax.get(e) map (_.responseText)
  }

  def fetchPage(title: String): Future[String] =
    Ajax.get("https://en.wikipedia.org/w/index.php?origin=*title=" + encodeURI(title)) map (_.responseText)

//  def fetchHTML(url: String) = Future { scala.io.Source.fromURL(url).mkString }

  def linksOn(html: String): Iterator[String] = {
    val pattern = """<a href="/wiki/([^:"]+)"""".r("title")
    val articles = for (m <- pattern.findAllMatchIn(html)) yield m.group("title")
    val catPattern = """<a href="/wiki/(Category:[^:"]+)"""".r("title")
    val categories = for (m <- catPattern.findAllMatchIn(html)) yield m.group("title")
    articles ++ categories
  }

  def getFirstImgUrl(html: String): Option[String] = {
    val pattern = """<a href="/wiki/([^"]+)" class="image"[^>]*>[^<]*<img[^>]*src="([^"]+)"""".r("file", "img")
    val file = pattern.findAllMatchIn(html).find(_.group("file") != "File:Question_book-new.svg")
    file map (_.group("img"))
  }

  def getDesc(html: String): Future[String] = {
    val wikiDataUrlPattern = """<a href="([^"]+)"[^>]*>Wikidata item</a>""".r("url")
    val wikiDataUrl = wikiDataUrlPattern.findFirstMatchIn(html)

    val desc = wikiDataUrl map { url =>
      val wikiDataPage = fetchHTML(url.group("url"))
      val descPattern = """<span class="wikibase-descriptionview-text">([^<]+)</span>""".r("desc")
      wikiDataPage map (descPattern.findFirstMatchIn(_)) map (_.get.group("desc"))
    }
    desc.getOrElse(Future(""))
  }

  def getTitle(html: String): String = {
    val pattern = """<title>(.*) - Wikipedia</title>""".r("title")
    pattern.findFirstMatchIn(html) match {
      case Some(m) => m.group("title")
      case _ => ""
    }
  }


  private def randomPage: Future[String] = {
    val random = fetchHTML(url + "Special:Random")

    val links = random map linksOn // grab all links on the random page

    // map those links to their html pages
    val pages = (links map { titles =>
      Future.sequence(titles map { t => fetchHTML(url + t)})
    }).flatten

    // find the page with the maximum number of links
    pages map { _.maxBy(linksOn(_).size) }
  }

  def getRandomPage: Future[Page] = {
    val page = randomPage
    val futureDesc = (page map getDesc).flatten

    // I use Seq here instead of Tuple2 because Scala yelled a lot about implicits and Nothing type
    Future.sequence(Seq(page, futureDesc)) map { seq =>
      val html = seq.head
      val desc = seq.last
      val links = linksOn(html)

      val pattern = """<a href="/wiki/[^"]+"[^>]+title="([^"]+)"[^>]+>([^<]+)</a>""".r("title", "text")
      val otherSpellings = pattern.findAllMatchIn(html) flatMap (m => List(m.group("title"), m.group("text")))

      val category = """Category:(.*)""".r
      val subArticle = """(.*)#(.*)""".r

      val items = (links ++ otherSpellings) flatMap {
        case category(title) => List(title)
        case subArticle(main, sub) => List(main, sub)
        case item => List(item)
      } map decodeURI map (_.toLowerCase.replace("_", " "))

      Page(getTitle(html), desc, items.toSet, getFirstImgUrl(html))
    }

  }

}

case class Page(title: String, desc: String, items: Set[String], image: Option[String])

