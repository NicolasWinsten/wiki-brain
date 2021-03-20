package com.nicolaswinsten.wikibrain

import org.scalajs.dom.ext.Ajax

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex
import scalajs.js.URIUtils.{decodeURI, encodeURI}

/**
 * web scraping object to work on wikipedia
 *
 * WARNING: I use regex to parse the HTML. Hopefully, I'll learn a good jQuery facade at some point
 */
object Scraper {
  val url = "https://en.wikipedia.org/wiki/"

  // Perform a GET on the given url and return a Future of its response
  def fetchHTML(url: String): Future[String] = {
    println("fetching " + url)

    // I ran into a CORS issue when trying to read Wikipedia from a JS XMLHttpRequest.
    // Never even heard about CORS but the only solution I could find to get around CORS was to setup a CORS proxy
    // with Heroku. Prefixing the url with this proxy will cause the request to be first routed through the proxy which
    // will forward the request, receive the response, and add the Access-Control-Allow-Origin header to the response,
    // which was not originally in Wikipedia's response. The proxy will then pass the response back here.
    val proxy = "https://still-woodland-82497.herokuapp.com/"
    Ajax.get(proxy + url) map (_.responseText)
  }

  // return a set of all the article and category titles linked on the given wikipage HTML
  def itemsOn(html: String): Set[String] = {
    val pattern = """<a href="/wiki/[^:"]+" title="([^"]+)">([^<]+)</a>""".r("title", "text")
    val articleMatches = pattern.findAllMatchIn(html).toSet flatMap ((m: Regex.Match) => Set(m.group("title"), m.group("text")))
    val catPattern = """<a href="/wiki/Category:[^"]*"[^>]*>([^<]*)</a>""".r("cat")
    val catMatches = catPattern.findAllMatchIn(html).toSet map ((m: Regex.Match) => m.group("cat"))

    val subArticle = "(.*)#(.*)".r
    val ambiguous = "(.*) \\((.*)\\)".r

    (articleMatches ++ catMatches) map decodeURI flatMap {
      case subArticle(main, sub) => Set(main, sub)
      case ambiguous(title, disamb) => Set(title, disamb)
      case t => Set(t)
    } map (_.toLowerCase.replaceAll("[^\\d\\w ]", "").trim)

  }

  // grab the first relevant image on the given wikipage html and return its url
  def getFirstImgUrl(html: String): Option[String] = {
    // does this regex upset you? does it hurt?
    val pattern = """<a href="/wiki/([^"]+)" class="image"[^>]*>[^<]*<img[^>]*src="([^"]+)"""".r("file", "img")
    val file = pattern.findAllMatchIn(html).find(_.group("file") != "File:Question_book-new.svg")
    file map (_.group("img"))
  }

  def getDesc(title: String): Future[String] = {
    val fixedTitle = title.replaceAll("&", "%26")
    val html = fetchHTML(s"https://en.wikipedia.org/w/index.php?title=$fixedTitle&action=info")
    val localDescPattern = """Local description</td>[^<]*<td>([^<]*)</td>""".r("desc")
    val centralDescPattern = """Central description</td>[^<]*<td>([^<]*)</td>""".r("desc")
    val matches = html map { html =>
      localDescPattern.findAllMatchIn(html) ++ centralDescPattern.findAllMatchIn(html)
    }
    matches map { iter =>
      iter map (_.group("desc")) maxByOption (_.length) getOrElse "No description found"
    }
  }

  def getTitle(html: String): String = {
    val pattern = """<title>(.*) - Wikipedia</title>""".r("title")
    pattern.findFirstMatchIn(html) match {
      case Some(m) => m.group("title")
      case _ => ""
    }
  }


  /**
   * @return Future of a random Page from wikipedia
   */
  def getPage(title: String): Future[Page] = {
    val page = fetchHTML(url + title)
    val futureDesc = getDesc(title)

    // I use Seq here instead of Tuple2 because Scala yelled a lot about implicits and Nothing type
    Future.sequence(Seq(page, futureDesc)) map { seq =>
      val html = seq.head
      val desc = seq.last
      Page(title.replaceAll("_", " "), desc, itemsOn(html), getFirstImgUrl(html))
    }

  }

  def getRandomPage: Future[Page] = {
    import scala.util.Random
    val i = (new Random).nextInt(articles.pool.length)
    val title = articles.pool(i)
    getPage(title)
  }

}

/**
 * This case class encapsulates info of a Wikipedia article
 *
 * @param title The normalized article title
 * @param desc A short description of the article subject if available. (might be empty string)
 * @param items set of "things" on the wikipage
 * @param image url to image on the page
 */
case class Page(title: String, desc: String, items: Set[String], image: Option[String])

