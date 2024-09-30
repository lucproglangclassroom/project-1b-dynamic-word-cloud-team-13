package hellotest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import scala.collection.mutable
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.language.unsafeNulls

class TopWordsSpec extends AnyFunSuite with Matchers with BeforeAndAfterEach {

  var wordsQueue: mutable.Queue[String] = _
  var wordCounts: mutable.Map[String, Int] = _

  override def beforeEach(): Unit = {
    wordsQueue = mutable.Queue[String]()
    wordCounts = mutable.Map[String, Int]()
  }

  test("parseArgs should correctly parse all arguments") {
    val args = Array("-c", "20", "-l", "5", "-w", "500")
    val config = TopWords.parseArgs(args)
    config.cloudSize shouldEqual 20
    config.minLength shouldEqual 5
    config.windowSize shouldEqual 500
  }

  test("parseArgs should use default values when arguments are missing") {
    val args = Array("-c", "15")
    val config = TopWords.parseArgs(args)
    config.cloudSize shouldEqual 15
    config.minLength shouldEqual 6 // default
    config.windowSize shouldEqual 1000 // default
  }

  test("getTopWords should correctly count and retrieve top words") {
    val newWords1 = Seq("hello", "world", "hello", "scala", "testing")
    val topWords1 = TopWords.getTopWords(wordsQueue, wordCounts, newWords1, cloudSize = 3, windowSize = 10)

    topWords1 should contain allOf ("hello" -> 2, "world" -> 1, "scala" -> 1)
    topWords1.length shouldBe 3

    val newWords2 = Seq("hello", "world", "scala", "java")
    val topWords2 = TopWords.getTopWords(wordsQueue, wordCounts, newWords2, cloudSize = 3, windowSize = 10)

    topWords2 should contain allOf ("hello" -> 3, "world" -> 2, "scala" -> 2)
    topWords2.length shouldBe 3
  }

  test("getTopWords should maintain the sliding window correctly") {
    val newWords1 = Seq("apple", "banana", "apple", "cherry", "banana")
    TopWords.getTopWords(wordsQueue, wordCounts, newWords1, cloudSize = 2, windowSize = 5)

    wordsQueue.size shouldBe 5
    wordCounts should contain allOf ("apple" -> 2, "banana" -> 2, "cherry" -> 1)

    val newWords2 = Seq("date", "apple", "banana")
    val topWords = TopWords.getTopWords(wordsQueue, wordCounts, newWords2, cloudSize = 2, windowSize = 5)

    wordsQueue.size shouldBe 5
    wordCounts should contain allOf ("banana" -> 2, "cherry" -> 1, "date" -> 1, "apple" -> 1)

    topWords.head shouldEqual ("banana" -> 2)
    topWords(1) should (equal ("cherry" -> 1) or equal ("date" -> 1) or equal ("apple" -> 1))
  }

  test("run should exit gracefully on empty input") {
    val inputLines = Seq.empty[String].iterator

    val outputStream = new ByteArrayOutputStream()
    val printStream = new PrintStream(outputStream)

    val config = Config()

    TopWords.run(inputLines, printStream, config)

    val actualOutput = outputStream.toString.trim.nn

    actualOutput shouldBe empty
  }

  // Additional Edge Case Tests

  test("run should handle zero cloudSize gracefully") {
    val inputLines = Seq("hello world hello").iterator

    val outputStream = new ByteArrayOutputStream()
    val printStream = new PrintStream(outputStream)

    val config = Config(cloudSize = 0, minLength = 3, windowSize = 5)

    TopWords.run(inputLines, printStream, config)

    val actualOutput = outputStream.toString.trim.nn

    actualOutput shouldBe empty
  }

  test("run should handle negative windowSize by treating it as zero") {
    val inputLines = Seq("hello world hello").iterator

    val outputStream = new ByteArrayOutputStream()
    val printStream = new PrintStream(outputStream)

    val config = Config(cloudSize = 2, minLength = 3, windowSize = -5)

    TopWords.run(inputLines, printStream, config)

    val expectedOutputs = Seq(
      "hello: 2 world: 1"
    )

    val actualOutputs = outputStream.toString.trim.nn.split("\n").map(_.trim).toSeq

    actualOutputs should have size expectedOutputs.size
    actualOutputs should contain theSameElementsInOrderAs expectedOutputs
  }
}