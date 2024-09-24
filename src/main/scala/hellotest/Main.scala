package hellotest

import scala.collection.mutable.{Map, Queue}

object Main {
  def main(args: Array[String]): Unit = {
    val cloudSize = args.headOption.map(_.toInt).getOrElse(10)
    val minLength = args.lift(1).map(_.toInt).getOrElse(1)
    val windowSize = args.lift(2).map(_.toInt).getOrElse(100)

    // Store words in lowercase for case-insensitive counting
    val wordCounts = Map.empty[String, Int] 
    val recentWords = Queue.empty[String]

    def updateWordCloud() = {
      val sortedCounts = wordCounts.toSeq.sortBy(-_._2).take(cloudSize)
      println(sortedCounts.map { case (word, count) => s"$word: $count" }.mkString(" "))
    }

    for (line <- scala.io.Source.stdin.getLines()) {
      line.split("\\s+").filter(_.nonEmpty).foreach { word =>
        if (word.length >= minLength) {
          // Convert word to lowercase before storing/counting
          val lowerCaseWord = word.toLowerCase
          wordCounts.update(lowerCaseWord, wordCounts.getOrElse(lowerCaseWord, 0) + 1)
          recentWords.enqueue(lowerCaseWord)
          if (recentWords.size > windowSize) {
            val removedWord = recentWords.dequeue()
            wordCounts.update(removedWord, wordCounts(removedWord) - 1)
          }
        }
      }
      updateWordCloud()
    }
  }
}