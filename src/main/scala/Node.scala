package eta

import scala.collection.mutable.HashMap
import scala.collection.immutable.ArraySeq
import scala.util.hashing.MurmurHash3

/** Position in a source file */
class Pos(val row: Int, val col: Int)

/** Location of a block of code in a source file */
class Loc(val file: String, val begin: Pos, val end: Pos) {
  def beginLoc() = Loc(file, end, end)
  def endLoc() = Loc(file, end, end)
}

/** Debug information for a node */
class Debug(val name: String = "", val loc: Option[Loc] = None) {
  def join(other: Debug) = Debug(
    if name.isEmpty then other.name else name,
    if loc.isDefined then loc else other.loc)
}

object Debug {
  def join(left: Option[Debug], right: Option[Debug]) = (left, right) match {
    case (Some(left), Some(right)) => Some(left.join(right))
    case (Some(_), _) => left
    case (_, Some(_)) => right
    case _ => None
  }
}

/** Base class for all Eta nodes, should be created via a [[Module]] */
sealed abstract class Node
  ( val operands: ArraySeq[Node]
  , val typeOrModule: Either[Module, Node]
  , val debug: Option[Debug])
{
  def ty = typeOrModule match {
    case Right(nodeType) => nodeType
    case _ => throw MatchError(typeOrModule)
  }

  def module: Module = {
    var node = this;
    while (true) {
      node.typeOrModule match {
        case Left(module) => return module
        case Right(nodeType) => node = nodeType
      }
    }
    return null // Never reached
  }

  def name = debug match {
    case Some(debug) => debug.name
    case _ => ""
  }

  def level: Int = ty match {
    case _: Uni => 0
    case _: Node => ty.level + 1
  }

  def replace(from: Node, to: Node) = replaceWith(HashMap() += (from -> to))

  def replaceWith(nodeMap: HashMap[Node, Node]): Node = {
    nodeMap.get(this) match {
      case Some(node) => node
      case _ => {
        if (this.isInstanceOf[Uni]) { return this }
        val newTy = ty.replaceWith(nodeMap)
        val newOps = operands.map((op: Node) => op.replaceWith(nodeMap))
        val newNode = rebuild(module, newTy, newOps, debug)
        nodeMap.addOne(this -> newNode)
        newNode
      }
    }
  }

  def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) = this

  def compare(other: Node) =
    other.getClass == getClass &&
    other.typeOrModule == typeOrModule &&
    other.operands == operands

  def hash = {
    var h = getClass.hashCode
    h = MurmurHash3.mix(h, typeOrModule.hashCode)
    h = MurmurHash3.mix(h, operands.hashCode)
    h
  }
}

class Uni   (module: Module) extends Node(ArraySeq(), Left(module),    None)
class Star  (nodeType: Node) extends Node(ArraySeq(), Right(nodeType), None)
class Nat   (nodeType: Node) extends Node(ArraySeq(), Right(nodeType), None)
class IntT  (nodeType: Node) extends Node(ArraySeq(), Right(nodeType), None)
class FloatT(nodeType: Node) extends Node(ArraySeq(), Right(nodeType), None)

class Lit[T](litType: Node, val value: T) extends Node(ArraySeq(), Right(litType), None) {
  override def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) =
    module.createLit[T](newTy, value)

  override def compare(other: Node) =
    super.compare(other) && (other match {
      case other: Lit[T] => other.value == value
      case _ => false
    })

  override def hash = MurmurHash3.mix(super.hash, value.hashCode)
}

class Pi(_param: Node, _codom: Node, debug: Option[Debug])
  extends Node(ArraySeq(_param, _codom), Right(_codom.ty), debug)
{
  override def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) =
    module.createPi(newOps(0), newOps(1), Debug.join(this.debug, debug))

  def param = operands(0)
  def domain = param.ty
  def codomain = operands(1)
}

class Abs(absType: Node, _param: Node, _body: Node, debug: Option[Debug])
  extends Node(ArraySeq(_param, _body), Right(absType), debug)
{
  override def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) =
    module.createAbs(newOps(0), newOps(1), Debug.join(this.debug, debug))

  def body = operands(0)
  def param = operands(1)
}

class App(appType: Node, _left: Node, _right: Node, debug: Option[Debug])
  extends Node(ArraySeq(_left, _right), Right(appType), debug)
{
  override def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) =
    module.createApp(newOps(0), newOps(1), Debug.join(this.debug, debug))

  def left = operands(0)
  def right = operands(1)
}

class Var(varType: Node, val id: Int, debug: Option[Debug])
  extends Node(ArraySeq(), Right(varType), debug)
{
  override def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) =
    module.createVar(newTy, id, Debug.join(this.debug, debug))
}

class Let(letType: Node, binders: ArraySeq[Node], values: ArraySeq[Node], body: Node, debug: Option[Debug])
  extends Node(binders ++ values ++ ArraySeq(body), Right(letType), debug)
{
  override def rebuild(module: Module, newTy: Node, newOps: ArraySeq[Node], debug: Option[Debug]) = {
    val binderCount = (newOps.length - 1) / 2
    module.createLet(
      newOps.slice(0, binderCount),
      newOps.slice(binderCount + 1, 2 * binderCount),
      newOps(binderCount * 2),
      Debug.join(this.debug, debug))
  }
}

/**
 * A module, holding all the hash-consed nodes in a hash map.
 * Nodes are simplified when created, and then stored into the hash map,
 * so that they are not simplified again when requested.
 */
class Module {
  private class HashedNode(val node: Node) {
    override def equals(other: Any) =
      other match {
        case other: HashedNode => other.node.compare(node)
        case _ => false
      }
    override def hashCode = node.hash
  }

  private var nodeMap: HashMap[HashedNode, Node] = HashMap()

  // TODO
  private def simplify(node: Node) = node

  private def hashCons(node: Node) = {
    val hashedNode = HashedNode(node)
    nodeMap.get(hashedNode) match {
      case Some(otherNode) => otherNode
      case _ => {
        val simplifiedNode = simplify(node)
        nodeMap += (hashedNode -> simplifiedNode)
        simplifiedNode
      }
    }
  }

  private val _uni  = hashCons(Uni(this))
  private val _star = hashCons(Star(uni))
  private val _nat  = hashCons(Nat(uni))
  private val intOrFloatType = createPi(_nat, _star)
  private val _intT   = hashCons(IntT(intOrFloatType))
  private val _floatT = hashCons(FloatT(intOrFloatType))

  def nodeCount = nodeMap.size

  def uni    = _uni
  def star   = _star
  def nat    = _nat
  def intT   = _intT
  def floatT = _floatT

  def createLit[T](litType: Node, value: T) = hashCons(Lit[T](litType, value))
  def createIntLit(intType: Node, value: BigInt) = createLit(intType, value)
  def createFloatLit(floatType: Node, value: BigDecimal) = createLit(floatType, value)
  def createNatLit(value: BigInt) = createIntLit(nat, value)

  def createSizedInt(bitwidth: BigInt) = createApp(intT, createNatLit(bitwidth))
  def createSizedFloat(bitwidth: BigInt) = createApp(floatT, createNatLit(bitwidth))
  def createVar(varType: Node, id: Int, debug: Option[Debug] = None) = hashCons(Var(varType, id, debug))

  def createPi(from: Node, to: Node, debug: Option[Debug] = None) = hashCons(Pi(from, to, debug))
  def createAbs(param: Node, body: Node, debug: Option[Debug] = None) = {
    val absType = createPi(param, body, debug)
    hashCons(Abs(absType, param, body, debug))
  }

  def createApp(left: Node, right: Node, debug: Option[Debug] = None) = {
    val appType = (left.ty: @unchecked) match { case left: Pi => left.codomain.replace(left.param, right) }
    hashCons(App(appType, left, right, debug))
  }

  def createLet(binders: ArraySeq[Node], values: ArraySeq[Node], body: Node, debug: Option[Debug] = None) = {
    val letType = body.ty.replaceWith(HashMap() ++= binders.zip(values))
    hashCons(Let(letType, binders, values, body, debug))
  }
}

object Module {
  type IntLit   = Lit[BigInt]
  type FloatLit = Lit[BigDecimal]
}
