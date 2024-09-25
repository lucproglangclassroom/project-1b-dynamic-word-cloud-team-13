package hellotest

import scala.collection.mutable.{Map, Queue}
import scala.language.unsafeNulls

object Main {
  def main(args: Array[String]): Unit = {
    val cloudSize = args.headOption.map(_.toInt).getOrElse(10)
    val minLength = args.lift(1).map(_.toInt).getOrElse(1)
    val windowSize = args.lift(2).map(_.toInt).getOrElse(100)
    val wordCounts = Map.empty[String, Int] 
    val recentWords = Queue.empty[String]

    def updateWordCloud() = {
      val sortedCounts = wordCounts.toSeq.sortBy(-_._2).take(cloudSize)
      println(sortedCounts.map { case (word, count) => s"$word: $count" }.mkString(" "))
    }

    for (line <- scala.io.Source.stdin.getLines()) {
      line.split("\\s+").filter(_ != null).filter(_.nonEmpty).foreach { word =>
        if (word.length >= minLength) {
          val lowerCaseWord = word.toLowerCase
          wordCounts.update(lowerCaseWord, wordCounts.getOrElse(lowerCaseWord, 0) + 1)
          recentWords.enqueue(lowerCaseWord)
          if (recentWords.size > windowSize) {
            val removedWord = recentWords.dequeue()
            wordCounts.update(removedWord, wordCounts(removedWord) - 1)
          }
        }
      }
      //to handle SIGPIPE
      if (System.out.checkError()) {
        System.exit(1)
      }

      updateWordCloud()
    }
  }
}