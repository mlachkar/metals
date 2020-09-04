package tests.codeactions

import scala.meta.internal.metals.codeactions.{ExplicitResultType}

class ExplicitResultTypeLspSuite
    extends BaseCodeActionLspSuite("ExplicitResultType") {

  check(
    "basic add explicit type annotation",
    """
      |package a
      |import scala.concurrent.duration._
      |
      |object A {
      |  val d<<>> = Duration(10, MICROSECONDS)
      |}
      |""".stripMargin,
    s"${ExplicitResultType.title}",
    """
      |package a
      |import scala.concurrent.duration._
      |
      |object A {
      |  val d: FiniteDuration = Duration(10, MICROSECONDS)
      |}
      |""".stripMargin.replace("'", "\""),
  )
  check(
    "basic add explicit type annotation 2",
    """
      |package a
      |object A {
      |  val d<<>> = ""
      |}
      |""".stripMargin,
    s"${ExplicitResultType.title}",
    """
      |package a
      |object A {
      |  val d: String = ""
      |}
      |}
      |""".stripMargin.replace("'", "\""),
  )
}
