package hellotest

import scala.io.StdIn
import scala.collection.mutable
import java.io.PrintStream
import scala.language.unsafeNulls

case class Config(cloudSize: Int = 10, minLength: Int = 6, windowSize: Int = 1000)

object TopWords {

  def parseArgs(args: Array[String]): Config = {
    args.sliding(2, 2).foldLeft(Config()) { (config, argPair) =>
      argPair match {
        case Array("-c", size)    => config.copy(cloudSize = size.toInt)
        case Array("-l", length)  => config.copy(minLength = length.toInt)
        case Array("-w", size)    => config.copy(windowSize = size.toInt)
        case _                    => config
      }
    }
  }

  def getTopWords(
      words: mutable.Queue[String],
      wordCounts: mutable.Map[String, Int],
      newWords: Seq[String],
      cloudSize: Int,
      windowSize: Int
  ): Seq[(String, Int)] = {
    newWords.foreach { word =>
      words.enqueue(word)
      wordCounts.updateWith(word) {
        case Some(count) => Some(count + 1)
        case None        => Some(1)
      }
    }

    if (windowSize > 0) {
      while (words.length > windowSize) {
        val removedWord = words.dequeue()
        wordCounts.updateWith(removedWord) {
          case Some(count) if count > 1 => Some(count - 1)
          case _                        => None
        }
        ()
      }
    }

    wordCounts.toSeq.sortBy(-_._2).take(cloudSize)
  }

  def printWordCloud(wordCounts: mutable.Map[String, Int], cloudSize: Int, out: PrintStream = System.out): Unit = {
    val topWords = wordCounts.toSeq.sortBy(-_._2).take(cloudSize)
    out.println(topWords.map { case (word, count) => s"$word: $count" }.mkString(" "))

    // Handle SIGPIPE
    if (out.checkError()) {
      System.exit(1)
    }
  }

  def run(
      input: Iterator[String],
      output: PrintStream,
      config: Config
  ): Unit = {
    val words = mutable.Queue[String]()
    val wordCounts = mutable.Map[String, Int]()

    input.foreach { line =>
      val validWords = line.split("\\s+").filter(_.length >= config.minLength).toSeq

      getTopWords(words, wordCounts, validWords, config.cloudSize, config.windowSize)
      printWordCloud(wordCounts, config.cloudSize, output)
    }
  }

  def main(args: Array[String]): Unit = {
    val config = parseArgs(args)
    val input = io.Source.stdin.getLines()
    val output = System.out
    run(input, output, config)
  }
}
