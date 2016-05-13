package dict

import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, Document}
import org.jsoup.select.Elements

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by gzq on 16-5-11.
  */
class word(word: String = "") {
  private val url = s"http://dict.youdao.com/w/$word/"
  private var web: Document = null
  private var pronounce: Elements = null

  private def getPronounce(country: String = "英") = {
    initWeb()
    initPronounce()
    pronounce.toList.find(_.ownText().startsWith(country)).map(_.child(0).ownText()).getOrElse("")
  }

  def uk = {
    getPronounce("英")
  }

  def us = {
    getPronounce("美")
  }

  def definition = {
    initWeb()
    val div = web.select("div#phrsListTab > div.trans-container")
    val pOrUl = div.last()
    if (pOrUl.hasClass("additional")) {
      val ul = div.first()
      ul.children().toList.map(_.text()).mkString("\n") + pOrUl.text()
    } else {
      pOrUl.children().toList.map(_.text()).mkString("\n")
    }
  }

  def network = {
    initWeb()
    val divs = web.select("div#tWebTrans > div.wt-container")
    val ps = web.select("div#tWebTrans > div#webPhrase > p.wordGroup")
    val definitions = divs.toList.map(_.children().toList.init.map(_.text()).mkString("\n\t")).mkString("\n")
    val phrases =  ps.toList.map(p => p.child(0).text() + ": " + p.ownText()).mkString("\n")
    definitions + "\n短语\n" + phrases
  }

  def technology = {
    initWeb()
    val titles = web.select("div#tPETrans-type-list > a").toList.map(_.text())
    val definitions = web.select("ul#tPETrans-all-trans > li").toList.map(_.text())
    titles zip definitions map {
      case (title, definition) => title + "\n\t" + definition
    } mkString "\n"
  }

  def eeDict = {
    initWeb()
    val div = web.select("div#tEETrans > div").get(0)
    var content = ""
    for (element <- div.children()) {
      val tagName = element.tagName()
      if (tagName == "h4") {
        content += element.text() + "\n"
      } else if (tagName == "ul") {
        content += element.children().toList
          .map {
            li =>
              if (li.child(1).tagName() == "ul") {
                val speech = li.child(0).text()
                val definition = li.child(1).children().toList.zipWithIndex.map {
                  case (childElement, index) =>
                    (index + 1).toString + ". " + childElement.children().toList.map(_.text()).mkString("\n")
                } mkString "\n"
                speech + "\n" + definition
              } else {
                li.child(0).text() + " " + li.child(1).text() + "\n" + li.child(2).text()
              }
          } mkString "\n"
        content += "\n"
      } else if (tagName == "p") {
        content += element.text() + "\n"
      }
    }
    content
  }

  def authDict = {
    initWeb()
    val div = web.select("div#authDictTrans").get(0)
    var content = ""
    content += div.child(0).text() + "\n"
    content += printUl(div.child(1))
    content
  }

  private def printUl(element: Element) = {
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
        case "span" => content += elem.text() + "\n"
        case "p" => content += elem.text() + "\n"
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
          val extraBlank = if (elem.children().size() == 0 || elem.child(0).tagName() != "span") "\n" else ""
          if (index == 0) {
            content += ("\t" * level) + elem.ownText() + extraBlank
          } else {
            content += ("\t" * level) + "%d. %s%s".format(index, elem.ownText(), extraBlank)
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
      web = Jsoup.connect(url)
        .userAgent("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.94 Safari/537.36")
        .timeout(3000)
        .get()
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
