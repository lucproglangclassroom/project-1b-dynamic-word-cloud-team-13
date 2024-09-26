package hellotest

import scala.io.StdIn
import scala.collection.mutable
import scala.language.unsafeNulls

object TopWords {

  // Helper method to parse command line arguments
  def parseArgs(args: Array[String]): (Int, Int, Int) = {
    val argMap = args.sliding(2, 2).collect {
      case Array("-c", size) => "cloudSize" -> size.toInt
      case Array("-l", length) => "minLength" -> length.toInt
      case Array("-w", size) => "windowSize" -> size.toInt
    }.toMap

    val cloudSize = argMap.getOrElse("cloudSize", 10)
    val minLength = argMap.getOrElse("minLength", 6)
    val windowSize = argMap.getOrElse("windowSize", 1000)
    (cloudSize, minLength, windowSize)
  }

  // Method to update and print the word cloud
  def printWordCloud(words: mutable.Queue[String], cloudSize: Int): Unit = {
    val wordCounts = words.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).take(cloudSize)
    println(wordCounts.map { case (word, count) => s"$word: $count" }.mkString(" "))

    // to handle SIGPIPE
    if (System.out.checkError()) {
      System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val (cloudSize, minLength, windowSize) = parseArgs(args)
    val words = mutable.Queue[String]()

    while (true) {
      val input = StdIn.readLine()
      if (input == null) sys.exit(0)
      val validWords = input.split("\\s+").filter(_.length >= minLength)

      words.enqueueAll(validWords)
      if (words.length > windowSize) {
        val excess = words.length - windowSize
        for (_ <- 1 to excess) words.dequeue()
        }


      if (words.nonEmpty) {
        printWordCloud(words, cloudSize)
        }
    }
  }
}
