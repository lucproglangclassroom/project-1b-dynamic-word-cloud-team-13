package hellotest

import scala.io.StdIn
import java.io.PrintStream
import scala.annotation.tailrec
import scala.language.unsafeNulls 

case class Config(
    cloudSize: Int = 10,
    minLength: Int = 6,
    windowSize: Int = 1000,
    ignoreCase: Boolean = true
)

case class State(
    window: List[String],
    wordCounts: Map[String, Int]
)

object State {
  val empty: State = State(Nil, Map.empty)
}

trait ArgParser {
  def parseArgs(args: Array[String]): Config = {
    @tailrec
    def helper(
        argsList: List[String],
        config: Config
    ): Config = argsList match {
      case "-c" :: size :: tail =>
        helper(tail, config.copy(cloudSize = size.toInt))
      case "-l" :: length :: tail =>
        helper(tail, config.copy(minLength = length.toInt))
      case "-w" :: size :: tail =>
        helper(tail, config.copy(windowSize = size.toInt))
      case "-i" :: tail =>
        helper(tail, config.copy(ignoreCase = true))
      case "--ignore-case" :: tail =>
        helper(tail, config.copy(ignoreCase = true))
      case "--case-sensitive" :: tail =>
        helper(tail, config.copy(ignoreCase = false))
      case Nil => config
      case _ =>
        System.err.nn.println("Invalid arguments")
        System.exit(1)
        config
    }

    helper(args.toList, Config())
  }
}

trait Normalizer {
  def normalizeWord(word: String, ignoreCase: Boolean): String = {
    val lowerCased = if (ignoreCase) word.toLowerCase else word
    lowerCased
      .replaceAll("""[\p{Punct}]""", "")
      .trim
  }
}

trait StateManagement {
  def updateState(
      state: State,
      newWords: Seq[String],
      config: Config
  ): State = {
    val updatedWindow = state.window ++ newWords
    val updatedWordCounts = newWords.foldLeft(state.wordCounts) { (counts, word) =>
      counts.updated(word, counts.getOrElse(word, 0) + 1)
    }

    if (config.windowSize > 0 && updatedWindow.size > config.windowSize) {
      val excess = updatedWindow.size - config.windowSize
      val (toRemove, remainingWindow) = updatedWindow.splitAt(excess)
      val finalWordCounts = toRemove.foldLeft(updatedWordCounts) { (counts, word) =>
        counts.get(word) match {
          case Some(1) => counts - word
          case Some(n) => counts.updated(word, n - 1)
          case None    => counts
        }
      }

      State(remainingWindow, finalWordCounts)
    } else {
      State(updatedWindow, updatedWordCounts)
    }
  }
}
trait CloudGenerator {
  def getTopWords(
      wordCounts: Map[String, Int],
      cloudSize: Int
  ): Seq[(String, Int)] = {
    wordCounts.toSeq
      .sortBy { case (word, count) => (-count, word) } // Sort by descending count, then ascending word
      .take(cloudSize)
  }
  def formatWordCloud(topWords: Seq[(String, Int)]): String = {
    topWords.map { case (word, count) => s"$word: $count" }.mkString(" ")
  }
}

trait OutputHandler {
  def printWordCloud(
      wordCloud: String,
      out: PrintStream = System.out.nn
  ): Unit = {
    out.println(wordCloud)
    // Handle SIGPIPE
    if (out.checkError()) {
      System.exit(1)
    }
  }
}

object TopWords extends ArgParser with Normalizer with StateManagement with CloudGenerator with OutputHandler {
  def processLines(
      lines: Iterator[String],
      config: Config
  ): Iterator[String] = {
    def stateTransition(state: State, line: String): State = {
      val rawWords = line.split("\\s+").nn
      val validWords = rawWords
        .map(word => normalizeWord(word, config.ignoreCase))
        .filter(word => word.length >= config.minLength)
        .toSeq
      updateState(state, validWords, config)
    }

    val states: Iterator[State] = lines.scanLeft(State.empty)(stateTransition)
    val processedStates = states.drop(1)
    processedStates.map { state =>
      val topWords = getTopWords(state.wordCounts, config.cloudSize)
      formatWordCloud(topWords)
    }
  }

  def run(
      input: Iterator[String],
      output: PrintStream,
      config: Config
  ): Unit = {
    val wordClouds = processLines(input, config)

    wordClouds.foreach { cloud =>
      printWordCloud(cloud, output)
    }
  }

  def main(args: Array[String]): Unit = {
    val config = parseArgs(args)
    val input = io.Source.stdin.getLines()
    val output = System.out.nn
    run(input, output, config)
  }
}