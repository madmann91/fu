package eta

import scala.collection.mutable.StringBuilder

class Printer {
  private val builder = StringBuilder()
  var indent = 0
  var tab = "    "

  def contents = builder.result()

  def print(s: String) = builder.append(s)
  def println(s: String) = {
    builder.append(s)
    nl()
  }
  def printMany[T](elems: IterableOnce[T], sep: String, f: T => Unit) = {
    val it = elems.iterator
    while (it.hasNext) {
      f(it.next())
      if (it.hasNext)
        builder.append(sep)
    }
  }

  def nl() = builder.append(tab * indent)
  def shift(i: Int = 1) = indent += i
}

extension (node: Node) def print(p: Printer): Unit = {
  def printVar(node: Node) = {
      node.print(p)
      p.print(": ")
      node.ty.print(p)
  }

  node match {
    case _: Uni    => p.print("Universe")
    case _: Star   => p.print("Type")
    case _: Nat    => p.print("Nat")
    case _: IntT   => p.print("Int")
    case _: FloatT => p.print("Float")
    case v: Var    => p.print(s"${v.name}_${v.id}")
    case lit: Module.IntLit   => p.print(s"${lit.value}")
    case lit: Module.FloatLit => p.print(s"${lit.value}")
    case pi: Pi => {
      p.print("Π")
      printVar(pi.param)
      p.print(" . ")
      pi.codomain.print(p)
    }
    case abs: Abs => {
      p.print("λ")
      printVar(abs.param)
      p.print(" . ")
      abs.body.print(p)
    }
    case app: App => {
      app.left.print(p)
      p.print(" ")
      if (app.right.isInstanceOf[App]) p.print("(")
      app.right.print(p)
      if (app.right.isInstanceOf[App]) p.print(")")
    }
    case let: Let => {
      p.print("let ")
      p.printMany(let.binders.zip(let.values), ", ", (b, v) => {
        printVar(b)
        p.print(" = ")
        v.print(p)
      })
      p.print(" in ")
      let.body.print(p)
    }
  }
}
