package io.shiftleft.joern

import org.scalatest.{Matchers, WordSpec}
import io.shiftleft.queryprimitives.steps.Implicits._

class MethodBodyTests extends WordSpec with Matchers {

  val code = """
                int foo(int x) {
                  if (x > 10) {
                    return 10;
                  } else {
                    return x;
                  }
                }
             """.stripMargin

  "should find AST for method and children of body" in
    new TestCpg().buildCpg(code) { cpg =>
      val numAstNodes = cpg.method.body.map(_.ast.l.size).l
      val numChildren = cpg.method.body.map(_.children.l.size).l
      numAstNodes.map(_ - 1) shouldBe numChildren
    }

  "should find blocks" in
  new TestCpg().buildCpg(code) { cpg =>
    cpg.method.body.children.block.map(_.children.l.map(_.getClass.getSimpleName)).l shouldBe
      List(List("Return", "Literal"), List("Return", "Identifier"))
  }



}
