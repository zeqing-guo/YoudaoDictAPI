package dict

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.Try

/**
  * Created by gzq on 16-5-11.
  */
class word(word: String = "") {
  private val url = s"http://dict.youdao.com/w/$word/"
  private var web: Document = null
  private var pronounce: Elements = null

  private def getPronounce(country: String = "英"): String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    initPronounce()
    pronounce.find(_.ownText().startsWith(country)).map(_.child(0).ownText()).getOrElse("")
  }

  def uk = {
    getPronounce("英")
  }

  def us = {
    getPronounce("美")
  }

  def simpleDefinition: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val div = web.select("div#phrsListTab > div.trans-container")
    if (div.size() == 0) {
      return ""
    }
    val pOrUl = div.last()
    if (pOrUl.hasClass("additional")) {
      val ul = div.first()
      ul.children().map(_.text()).mkString("\n") + pOrUl.text()
    } else {
      pOrUl.children().map(_.text()).mkString("\n")
    }
  }

  def networkDefinition: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#tWebTrans > div.wt-container")
    if (divs.size() == 0) {
      return ""
    }
    val ps = web.select("div#tWebTrans > div#webPhrase > p.wordGroup")
    val definitions = divs.map(_.children().toList.init.map(_.text()).mkString("\n\t")).mkString("\n")
    val phrases =  ps.map(p => p.child(0).text() + ": " + p.ownText()).mkString("\n")
    definitions + "\n短语\n" + phrases
  }

  def glossaryDefinition: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val titles = web.select("div#tPETrans-type-list > a").map(_.text())
    if (titles.isEmpty) {
      return ""
    }
    val definitions = web.select("ul#tPETrans-all-trans > li").map(_.text())
    titles zip definitions map {
      case (title, definition) => title + "\n\t" + definition
    } mkString "\n"
  }

  def eeDefinition: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#tEETrans > div")
    if (divs.size() == 0) {
      return ""
    }
    val div = divs.head
    var content = ""
    content += div.child(0).text() + "\n"
    content += getPrettyUl(div.child(1))
    content
  }

  def authDict: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#authDictTrans")
    if (divs.size() == 0) {
      return ""
    }
    val div = divs.head
    var content = ""
    content += div.child(0).text() + "\n"
    content += getPrettyUl(div.child(1))
    content
  }

  def collinsDict: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#collinsResult > div > div > div > div")
    if (divs.size() == 0) {
      return ""
    }
    val div = divs.head
    var content = ""
    content += div.child(0).text() + "\n"
    div.child(1).removeClass("ol")
    content += getPrettyUl(div.child(1))
    content
  }

  def groups: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#wordGroup")
    if (divs.size() == 0) {
      return ""
    }
    val div = divs.head
    div.children().init.map {
      p => p.child(0).text() + " " + p.ownText()
    } mkString "\n"
  }

  def synonyms: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val uls = web.select("div#synonyms > ul")
    if (uls.size() == 0) {
      return ""
    }
    val ul = uls.head
    getPrettyUl(ul)
  }

  def relWords: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#relWordTab")
    if (divs.size() == 0) {
      return ""
    }
    val div = divs.head
    val textNodes = div.textNodes().filter(_.text().matches("\\s*\\S+\\s*"))
    val ps = div.children()
    ps.head.text() + "\n" + (ps.tail.zip(textNodes).map { case (p, t) => t.text().trim + "\n" + p.text() } mkString "\n")
  }

  def biSentences: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#bilingual > ul")
    if (divs.size() == 0) {
      return ""
    }
    getPrettyUl(divs.head)
  }

  def authSentences: String = {
    initWeb()
    if (web == null) {
      return "Connection timed out"
    }
    val divs = web.select("div#authority > ul")
    if (divs.size() == 0) {
      return ""
    }
    getPrettyUl(divs.head)
  }

  private def getPrettyUl(element: Element) = {
    var content = ""
    val elementStack = mutable.Stack[Element]()
    val indexStack = mutable.Stack[Int]()
    val levelStack = mutable.Stack[Int]()
    elementStack.push(element)
    levelStack.push(0)
    while (elementStack.nonEmpty) {
      val elem = elementStack.pop()
      val level = levelStack.pop()
      elem.tagName() match {
        case "span" =>
          val tail =
            if (Try(elem.nextElementSibling().tagName()).getOrElse("") == "span")
              " "
            else
              "\n"
          content += elem.text() + tail
        case "p" => content += " " * level + elem.text() + "\n"
        case "div" => content += " " * level + elem.text() + "\n"
        case "ul" =>
          if (elem.hasClass("ol")) {
            elem.children.zipWithIndex.reverse.foreach {
              case (li, index) =>
                elementStack.push(li)
                indexStack.push(index + 1)
                levelStack.push(level + 1)
            }
          } else {
            elem.children.reverse.foreach {
              li =>
                elementStack.push(li)
                indexStack.push(0)
                levelStack.push(level + 1)
            }
          }
        case "li" =>
          val index = indexStack.pop()
          val extraBlank =
            if (elem.children().size() == 0
              || elem.child(0).tagName() != "span" && elem.child(0).tagName() != "ul"
              || elem.child(0).tagName() == "ul" && elem.child(0).child(0).tagName() != "span")
              "\n"
            else
              ""
          if (index == 0) {
            content += (" " * level) + elem.ownText() + extraBlank
          } else {
            content += (" " * level) + "%d. %s%s".format(index, elem.ownText(), extraBlank)
          }
          elem.children.reverse.foreach {
            child =>
              elementStack.push(child)
              levelStack.push(level + 1)
          }
      }
    }
    content
  }

  private def initWeb() = {
    if (web == null) {
      web = Try(Jsoup.connect(url)
        .userAgent("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.94 Safari/537.36")
        .timeout(3000)
        .get()).getOrElse(null)
    }
  }

  private def initPronounce() = {
    if (pronounce == null) {
      pronounce = web.getElementsByClass("pronounce")
    }
  }
}

object word {
  def apply(word: String = ""): word = new word(word)
}
