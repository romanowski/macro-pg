
import scala.quoted.*
import scala.compiletime.*
import scala.deriving.*

trait Setter[T, Path <: Tuple]:
  def set(e: T, v: Any): T

object Setter:

  inline def indexForName[C <: Tuple, Name](index: Int = 0): Int =
    inline erasedValue[C] match
      case _: (Name *: ts) => index
      case _:(_ *: ts) =>
        indexForName[ts, Name](index + 1)
      case _: EmptyTuple => -1 // should not happen
        
  
  inline def setImpl[C <: Tuple, OtherNames <: Tuple](setAt: Int, index: Int, v: Any, p: Product): Tuple =
    inline erasedValue[C] match
      case _: (t *: ts) =>
        val nV = 
          if setAt == index then 
            val nestedE = p.productElement(index).asInstanceOf[t]
            summonInline[Setter[t, OtherNames]].set(nestedE, v) 
          else p.productElement(index)
        nV *: setImpl[ts, OtherNames](setAt, index + 1, v, p)
      case _: EmptyTuple => EmptyTuple

  given [T]: Setter[T, EmptyTuple] with
    def set(e: T, v: Any): T = v.asInstanceOf[T]

  inline given derive[T, Name, OtherNames <: Tuple](using m: Mirror.ProductOf[T]): Setter[T, Name *: OtherNames] = 
    inline m match
      case p: Mirror.ProductOf[T] =>
        new Setter[T, Name *: OtherNames]:
          def set(e: T, v: Any) = 
            
              val index = indexForName[m.MirroredElemLabels, Name](0)
              println(index)
              p.fromProduct(setImpl[m.MirroredElemTypes, OtherNames](index, 0, v, e.asInstanceOf[Product]))


def withPath[T: Type, R: Type](x: Expr[T => ?], compute: Expr[(Seq[String], Setter[T, _]) => R])(using Quotes): Expr[R] =
  import quotes.reflect.*

  def isCaseVal(term: Term, name: String) =
    term.tpe.typeSymbol.memberField(name).flags.is(Flags.CaseAccessor)

  def unwrap(a: Tree): Expr[Tuple] =
    a match 
      case Ident(a) if a.startsWith("_$") =>
        '{EmptyTuple}
      case Select(term, name) =>
        if isCaseVal(term, name) then '{${unwrap(term)} :* ${Expr(name)} }
        else report.errorAndAbort("Expected only case class fields", term.pos)
      case _ =>
        report.errorAndAbort(s"Unknown tree: ${a}", a.pos)

  x.asTerm match
    case Inlined(_, _, Block(List(DefDef(_, _, _, Some(term))), _))=>
      val names = unwrap(term)
      val tr = names.asTerm.tpe.asType
      
      '{
        println($names)
        val setter = summon[Setter[T, tr.Underlying]] // Tu nie działa
        ${compute}(${names}.productElementNames.toSeq, ???)
      }
    case expr =>
      report.errorAndAbort("TODO")