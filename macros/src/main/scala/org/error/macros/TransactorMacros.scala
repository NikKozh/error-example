package org.error.macros

import org.error.common.Transactor
import scala.quoted.*

/**
 *https://softwaremill.com/scala-3-macros-tips-and-tricks/
 *https://github.com/lampepfl/dotty-macro-examples
 * debug macros via sbt ~compile
 */
object TransactorMacros {

  inline def transactional[F[_], A](query: F[A])(transactor: Transactor[F]): F[A] = ${ transactionalImpl('query)('transactor) }

  def transactionalImpl[F[_], A](query: Expr[F[A]])(transactor: Expr[Transactor[F]])(using Quotes, Type[F], Type[A]): Expr[F[A]] =
    import quotes.reflect.*
    val methodTree = resolveMethod

    //tree branches https://www.scala-lang.org/api/3.x/scala/quoted/Quotes$reflectModule.html
    def findInsertClauses(tree: Tree): Boolean =
      tree match
        case Select(Ident("database"),"insert") => true
        case DefDef(_,_,_,Some(tree)) => findInsertClauses(tree)
        case Block(first, second) =>  first.exists(findInsertClauses) || findInsertClauses(second)
        case Apply(tree, args) =>  findInsertClauses(tree) || args.exists(findInsertClauses)
        case TypeApply(tree, args) =>  findInsertClauses(tree) || args.exists(findInsertClauses)
        case Ident(_) =>  false
        case _ => false

    //https://docs.scala-lang.org/scala3/guides/macros/reflection.html#printing-the-trees
    if(findInsertClauses(methodTree))
      report.info(s"Invoke into a master transaction with tree: ${methodTree.show(using Printer.TreeStructure)}")
      '{
        $transactor.transactWithMaster($query)
      }
    else
      report.info(s"Invoke into a replicas transaction with tree: ${methodTree.show(using Printer.TreeStructure)}")
      '{
        $transactor.transactWithReplica($query)
      }

  /**
   * example https://github.com/lampepfl/dotty-macro-examples/blob/main/outOfScopeMethodCall/src/macro.scala
   */
  def resolveMethod(using Quotes): quotes.reflect.Tree =
    import quotes.reflect.*
    var sym = Symbol.spliceOwner // symbol of method where the macro is expanded
    while sym != null && !sym.isDefDef do
      sym = sym.owner // owner of a symbol is what encloses it: e.g. enclosing method or enclosing class
    sym.tree //TODO need to change in a prod https://docs.scala-lang.org/scala3/guides/macros/best-practices.html#avoid-symboltree

}
