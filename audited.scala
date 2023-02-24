//> using scala "3.2.2"
import scala.reflect.ClassTag

case class LeafA(
  a: String
)

case class LeafB(
  b: String
)

case class NodeA(
  a: LeafA,
  b: LeafB
)

case class Tree(
  name: String,
  node: NodeA
)

case class Audited(val a: Tree, audit: Map[Seq[String], Seq[String]]):

  def update(names: Seq[String], s: Setter[Tree, _]) = 
    println(names)
    a
  
  inline def update[T](inline op: Tree => T)(using T <:< Any)(v: T)(reason: String): Tree = 
    ${withPath('op, 'update)}


@main def auditedMain =
  val base = Audited(Tree("ala", NodeA(LeafA("a1"), LeafB("b2"))), Map.empty)
  println(base.update(_.name)("ola")("nameChange"))

  println(base.update(_.node.a.toString)("ola")("nameChange"))

  


