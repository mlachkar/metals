package scala.meta.internal.metals.codeactions

import org.eclipse.lsp4j.CodeActionParams

import scala.concurrent.{ExecutionContext, Future}
import scala.meta.internal.metals.{CodeAction, ScalafixProvider}
import scala.meta.pc.CancelToken
import org.eclipse.{lsp4j => l}
import scala.meta.internal.metals.MetalsEnrichments._

import scala.meta.internal.metals.MetalsEnrichments.XtensionString

final class ExplicitResultType(scalafixProvider: ScalafixProvider)
    extends CodeAction {

  /**
   * This should be one of the String constants
   * listed in [[org.eclipse.lsp4j.CodeActionKind]]
   */
  override def kind: String = l.CodeActionKind.RefactorRewrite

  override def contribute(params: CodeActionParams, token: CancelToken)(implicit
      ec: ExecutionContext
  ):Future[Seq[l.CodeAction]] = {

    scribe.info("I m here")
    val uri = params.getTextDocument.getUri
    val path = uri.toAbsolutePath
    val range: l.Range = params.getRange
    val edits = scalafixProvider.explicitResultType(path, range)

    val codeAction = new l.CodeAction()
    codeAction.setTitle(ExplicitResultType.title)
    codeAction.setKind(kind)
    codeAction.setEdit(
      new l.WorkspaceEdit(
        Map(uri -> edits.asJava).asJava
      )
    )
    Future.successful {
      Seq(codeAction)
    }
  }
}
object ExplicitResultType {
  val title: String = "Add type annotation to definition"
}
