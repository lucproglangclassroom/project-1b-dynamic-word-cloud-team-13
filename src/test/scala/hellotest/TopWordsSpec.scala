package hellotest

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import scala.collection.immutable.{Queue, Map}
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.language.unsafeNulls

class TopWordsSpec extends AnyFunSuite with Matchers with BeforeAndAfterEach {

  var wordsQueue: Queue[String] = _
  var wordCounts: Map[String, Int] = _

  override def beforeEach(): Unit = {
    wordsQueue = Queue[String]()
    wordCounts = Map[String, Int]()
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
    val config = Config(cloudSize = 3, minLength = 1, windowSize = 1000, ignoreCase = true)
    val newWords1 = Seq("hello", "world", "hello", "scala", "testing")
    val updatedState1 = TopWords.updateState(State.empty, newWords1, config)
    val topWords1 = TopWords.getTopWords(updatedState1.wordCounts, config.cloudSize)

    topWords1 should contain allOf ("hello" -> 2, "scala" -> 1, "testing" -> 1)
    topWords1.length shouldBe 3

    val newWords2 = Seq("hello", "world", "scala", "java")
    val updatedState2 = TopWords.updateState(updatedState1, newWords2, config)
    val topWords2 = TopWords.getTopWords(updatedState2.wordCounts, config.cloudSize)
  
    topWords2 should contain allOf ("hello" -> 3, "world" -> 2, "scala" -> 2)
    topWords2.length shouldBe 3
  }

  test("getTopWords should maintain the sliding window correctly") {
  val config = Config(cloudSize = 2, minLength = 1, windowSize = 5, ignoreCase = true)
  val newWords1 = Seq("apple", "banana", "apple", "cherry", "banana")
  val updatedState1 = TopWords.updateState(State.empty, newWords1, config)

  updatedState1.window.size shouldBe 5
  updatedState1.wordCounts should contain allOf ("apple" -> 2, "banana" -> 2, "cherry" -> 1)

  val newWords2 = Seq("date", "apple", "banana")
  val updatedState2 = TopWords.updateState(updatedState1, newWords2, config)

  updatedState2.window.size shouldBe 5
  updatedState2.wordCounts should contain allOf ("banana" -> 2, "cherry" -> 1, "date" -> 1, "apple" -> 1)

  val topWords = TopWords.getTopWords(updatedState2.wordCounts, config.cloudSize)
  
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