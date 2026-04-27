package ex1

import ex1.Parsers.charParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class ParserScalaTest extends AnyFlatSpec:
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z'))
    with NotTwoConsecutive[Char]
    with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser

  "BasicParser" should "work" in:
    parser.parseAll("aabc".toList) should be(true)
    parser.parseAll("aabcdc".toList) should be(false)
    parser.parseAll("".toList) should be(true)

  "NonEmptyParser" should "reject empty and accept valid sequences" in:
    parserNE.parseAll("0101".toList) should be(true)
    parserNE.parseAll("0123".toList) should be(false)
    parserNE.parseAll(List()) should be(false)

  "NotTwoConsecutiveParser" should "reject consecutive equals and accept valid sequences" in:
    parserNTC.parseAll("XYZ".toList) should be(true)
    parserNTC.parseAll("XYYZ".toList) should be(false)
    parserNTC.parseAll("".toList) should be(true)

  "NotEmpty and NotTwoConsecutive combined" should "enforce both constraints" in:
    parserNTCNE.parseAll("XYZ".toList) should be(true)
    parserNTCNE.parseAll("XYYZ".toList) should be(false)
    parserNTCNE.parseAll("".toList) should be(false)

  "String (char) parser" should "behave like BasicParser for strings" in:
    sparser.parseAll("aabc".toList) should be(true)
    sparser.parseAll("aabcdc".toList) should be(false)
    sparser.parseAll("".toList) should be(true)
