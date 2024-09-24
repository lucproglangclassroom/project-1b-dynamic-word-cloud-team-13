package hellotest

import scala.collection.mutable.{Map, Queue}

object Main {
  def main(args: Array[String]): Unit = {
    val cloudSize = args.headOption.map(_.toInt).getOrElse(10)
    val minLength = args.lift(1).map(_.toInt).getOrElse(3)
    val windowSize = args.lift(2).map(_.toInt).getOrElse(100)
    val minFrequency = args.lift(3).map(_.toInt).getOrElse(3)

    if (cloudSize <= 0 || minLength <= 0 || windowSize <= 0 || minFrequency <= 0) {
      println("Invalid arguments. Please provide positive integers for cloudSize, minLength, windowSize, and minFrequency.")
      return
    }

    val wordCounts = Map.empty[String, Int]
    val recentWords = Queue.empty[String]

    def updateWordCloud() = {
      val sortedCounts = wordCounts.toSeq.sortBy(-_._2).filter(_._2 >= minFrequency).take(cloudSize)
      val maxWordLength = sortedCounts.map(_._1.length).max
      println(sortedCounts.map { case (word, count) => f"$word${" " * (maxWordLength - word.length)}: $count" }.mkString(" "))
    }

    for (line <- scala.io.Source.stdin.getLines()) {
      line.split("\\s+").filter(_ != null).filter(_.nonEmpty).foreach { word =>
        if (word.length >= minLength) {
          wordCounts.update(word, wordCounts.getOrElse(word, 0) + 1)
          recentWords.enqueue(word)
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