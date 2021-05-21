import scala.collection.immutable.ArraySeq
import eta.print

@main def main: Unit = {
  val module = eta.Module()
  val (_1, _2) = (module.createNatLit(1), module.createNatLit(2))
  val (x, y) = (module.createVar(module.nat, 0, Some(eta.Debug("x"))), module.createVar(module.nat, 1, Some(eta.Debug("y"))))
  val let = module.createLet(ArraySeq(x, y), ArraySeq(_1, _2), x)
  val printer = eta.Printer()
  let.print(printer)
  println(printer.contents)
}
