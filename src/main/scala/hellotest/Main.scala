package hellotest

import scala.collection.mutable.{Map, Queue}

object Main {
  def main(args: Array[String]): Unit = {
    val cloudSize = args.headOption.map(_.toInt).getOrElse(10)
    val minLength = args.lift(1).map(_.toInt).getOrElse(3)
    val windowSize = args.lift(2).map(_.toInt).getOrElse(100)

    val wordCounts = Map.empty[String, Int]
    val recentWords = Queue.empty[String]

    def updateWordCloud() = {
      val sortedCounts = wordCounts.toSeq.sortBy(-_._2).take(cloudSize)
      println(sortedCounts.map { case (word, count) => s"$word: $count" }.mkString(" "))
    }

    for (word <- scala.io.Source.stdin.getLines()) {
      if (word.length >= minLength) {
        wordCounts.update(word, wordCounts.getOrElse(word, 0) + 1)
        recentWords.enqueue(word)
        if (recentWords.size > windowSize) {
          val removedWord = recentWords.dequeue()
          wordCounts.update(removedWord, wordCounts(removedWord) - 1)
        }
        updateWordCloud()
      }
    }
  }
}
