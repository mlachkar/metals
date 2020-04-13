package scala.meta.internal.metals

import org.eclipse.lsp4j.DocumentOnTypeFormattingParams
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextEdit

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.meta.inputs.Input
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.tokens.Token
import scala.meta.tokens.Token.{Constant, Interpolation}
import scala.meta.tokens.Tokens
import org.eclipse.lsp4j.DocumentRangeFormattingParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.Position

final class MultilineStringFormattingProvider(
    buffer: Buffers
)(implicit ec: ExecutionContext) {

  private val quote = '"'
  private val space = " "
  private val stripMargin = "stripMargin"

  private def hasStripMarginSuffix(
      stringTokenIndex: Int,
      tokens: Tokens
  ): Boolean = {
    var methodIndex = stringTokenIndex + 1
    while (tokens(methodIndex).isWhiteSpaceOrComment ||
      tokens(methodIndex).isInstanceOf[Token.Dot]) methodIndex += 1
    tokens(methodIndex) match {
      case token: Token.Ident if token.value == stripMargin =>
        true
      case _ =>
        false
    }
  }

  private def determineDefaultIndent(
      lines: Array[String],
      lineNumberToCheck: Int
  ): String = {
    val lineToCheck = lines(lineNumberToCheck)
    val index =
      if (lineToCheck.contains("\"\"\"|")) {
        lineToCheck.indexOf('"') + 3
      } else if (lineToCheck.contains("\"\"\"")) {
        lineToCheck.indexOf('"') + 2
      } else lineToCheck.indexOf('|')
    space * index
  }

  private def indent(
      sourceText: String,
      position: Position
  ): TextEdit = {
    val splitLines = sourceText.split('\n')
    // position line -1 since we are checking the line before when doing onType
    val defaultIndent = determineDefaultIndent(splitLines, position.getLine - 1)
    val existingSpaces = position.getCharacter()
    val addedSpaces = defaultIndent.drop(existingSpaces)
    val startChar = defaultIndent.size - addedSpaces.size
    position.setCharacter(startChar)
    val endChar = startChar + Math.max(0, existingSpaces - defaultIndent.size)
    val endPosition = new Position(position.getLine(), endChar)
    new TextEdit(new Range(position, endPosition), addedSpaces + "|")
  }

  private def isMultilineString(text: String, token: Token): Boolean = {
    val start = token.start
    text(start) == quote &&
    text(start + 1) == quote &&
    text(start + 2) == quote
  }

  private def inToken(
      startPos: meta.Position,
      endPos: meta.Position,
      token: Token
  ): Boolean =
    startPos.startLine >= token.pos.startLine && endPos.endLine <= token.pos.endLine

  private def pipeInScope(
      startPos: meta.Position,
      endPos: meta.Position,
      text: String,
      newlineAdded: Boolean
  ): Boolean = {
    val indexOfLastBackToLine = text.lastIndexBetween(
      '\n',
      0,
      upperBound = startPos.start - 1
    )
    val lastBackToLine =
      if (!newlineAdded) indexOfLastBackToLine
      else
        text.lastIndexBetween('\n', 0, upperBound = indexOfLastBackToLine - 1)
    val pipeBetweenLastLineAndPos = text.lastIndexBetween(
      '|',
      lastBackToLine,
      startPos.start - 1
    )
    val pipeBetweenSelection = text.lastIndexBetween(
      '|',
      startPos.start - 1,
      endPos.end - 1
    )
    pipeBetweenLastLineAndPos != -1 || pipeBetweenSelection != -1
  }

  def extractTokensInRange(
      tokens: Tokens,
      start: meta.Position,
      end: meta.Position
  ): List[(Token, Int)] =
    tokens.zipWithIndex.filter {
      case (token, _) =>
        inToken(start, end, token)
    }.toList

  private def multilineStringInTokens(
      tokens: Tokens,
      startPos: meta.Position,
      endPos: meta.Position,
      sourceText: String,
      newlineAdded: Boolean
  ): Boolean = {
    if (pipeInScope(startPos, endPos, sourceText, newlineAdded)) {
      def shouldFormatMultiString: PartialFunction[(Token, Int), Boolean] = {
        case (token: Constant.String, index: Int) =>
          isMultilineString(sourceText, token) && hasStripMarginSuffix(
            index,
            tokens
          ) && inToken(startPos, endPos, token)
      }

      def shouldFormatInterpolationString
          : PartialFunction[(Token, Int), Boolean] = {
        case (token: Interpolation.Start, index: Int)
            if token.start < startPos.start => {
          var endIndex = index + 1
          while (!tokens(endIndex)
              .isInstanceOf[Interpolation.End]) endIndex += 1
          isMultilineString(sourceText, token) && hasStripMarginSuffix(
            endIndex,
            tokens
          )
        }
      }
      tokens.zipWithIndex
        .exists(
          shouldFormatMultiString orElse shouldFormatInterpolationString orElse {
            case _ => false
          }
        )
    } else false
  }

  private def withTokenOpt(textId: TextDocumentIdentifier, range: Range)(
      fn: (
          meta.Position,
          meta.Position,
          String,
          Option[Tokens]
      ) => List[TextEdit]
  ): Future[List[TextEdit]] = {
    val source = textId.getUri.toAbsolutePath
    if (source.exists) {
      val sourceText = buffer.get(source).getOrElse("")
      val virtualFile = Input.VirtualFile(source.toString(), sourceText)
      val startPos = range.getStart.toMeta(virtualFile)
      val endPos = range.getEnd.toMeta(virtualFile)
      Future { fn(startPos, endPos, sourceText, virtualFile.tokenize.toOption) }
    } else Future.successful(Nil)
  }
  private def doubleQuoteNotClosed(
      sourceText: String,
      position: Position
  ): Boolean = {
    val splitLines = sourceText.split('\n')
    val lineBefore = splitLines(position.getLine - 1)
    lineBefore.count(_ == '"') % 2 != 0
  }

  private def tryToIndent(
      sourceText: String,
      newlineAdded: Boolean,
      position: Position
  ): List[TextEdit] = {
    if (newlineAdded && doubleQuoteNotClosed(sourceText, position)) {
      val precedentLine = position.getLine - 1
      val lines = sourceText.split('\n')
      val lineText = lines(precedentLine)
      val lastChar = lineText.size
      val endPrecedentLine = new Position(precedentLine, lastChar)
      val textEditPrecedentLine = new TextEdit(
        new Range(endPrecedentLine, endPrecedentLine),
        "\"" + " " + "+"
      )
      val textEditcurrentLine =
        new TextEdit(new Range(position, position), "\"")
      List(textEditPrecedentLine, textEditcurrentLine)
    } else Nil
  }
  private def formatPipeLine(
      line: Int,
      lines: Array[String],
      defaultIndent: String
  ): Option[TextEdit] = {
    val zeroPos = new Position(line, 0)
    val lineText = lines(line)
    val firstChar = lineText.trim.headOption
    firstChar match {
      case Some('|') =>
        val firstPipeIndex = lineText.indexOf('|')
        val firstCharAfterPipe = lineText.trim.tail.trim.headOption

        firstCharAfterPipe match {
          case Some('|') =>
            val secondPipeIndex = lineText.indexOf('|', firstPipeIndex + 1)
            val secondPipePos = new Position(line, secondPipeIndex)
            val textEdit =
              new TextEdit(new Range(zeroPos, secondPipePos), defaultIndent)
            Some(textEdit)
          case _ =>
            val pipePos = new Position(line, firstPipeIndex)
            val textEdit =
              new TextEdit(new Range(zeroPos, pipePos), defaultIndent)
            Some(textEdit)
        }
      case _ =>
        val isFirstLineOfMultiLine = lineText.trim.contains("\"\"\"")
        if (isFirstLineOfMultiLine) {
          None
        } else {
          val newText = defaultIndent + "|"
          val textEdit = new TextEdit(new Range(zeroPos, zeroPos), newText)
          Some(textEdit)
        }
    }
  }

  def format(params: DocumentOnTypeFormattingParams): Future[List[TextEdit]] = {
    val range = new Range(params.getPosition, params.getPosition)
    val doc = params.getTextDocument()
    val newlineAdded = params.getCh() == "\n"
    withTokenOpt(doc, range) { (startPos, endPos, sourceText, tokens) =>
      tokens match {
        case Some(tokens) =>
          if (multilineStringInTokens(
              tokens,
              startPos,
              endPos,
              sourceText,
              newlineAdded
            )) {
            List(indent(sourceText, params.getPosition))
          } else Nil
        case None => tryToIndent(sourceText, newlineAdded, params.getPosition)
//          if (stringInTokens(params.getPosition, sourceText)) {
//            indentString(sourceText, newlineAdded, params.getPosition)
      }
    }
  }

  def format(params: DocumentRangeFormattingParams): Future[List[TextEdit]] = {
    val range = params.getRange()
    val doc = params.getTextDocument()
    withTokenOpt(doc, range) { (startPos, endPos, sourceText, tokens) =>
      val splitLines = sourceText.split('\n')
      // position.startLine since we want to check current line on rangeFormatting
      val defaultIndent = determineDefaultIndent(splitLines, startPos.startLine)
      tokens match {
        case Some(tokens) =>
          if (multilineStringInTokens(
              tokens,
              startPos,
              endPos,
              sourceText,
              false
            )) {
            val linesToFormat =
              range.getStart().getLine().to(range.getEnd().getLine())
            linesToFormat
              .flatMap(line => formatPipeLine(line, splitLines, defaultIndent))
              .toList
          } else Nil
        case None => Nil
      }
    }
  }

}
