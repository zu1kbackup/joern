import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Member, Method}
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

@main def main() = {
  cpg.method
    .internal
    .filter { method =>
      method
        .start
        .assignments
        .target
        .reachableBy(method.parameter.filter(_.typeFullName.contains("const")))
        .nonEmpty
  }.toSetImmutable
}
