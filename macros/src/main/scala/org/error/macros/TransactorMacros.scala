package org.error.macros

import scala.annotation.{MacroAnnotation, experimental}
import scala.quoted.*

object TransactorMacros {

  @experimental
  class rewriteDefaultTransactorCalls extends MacroAnnotation {

    override def transform(using quotes: Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = {
      import quotes.reflect.*

      class TreeMapper(resultTransactMethod: String) extends TreeMap {
        override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term =
          tree match {
            case Select(qualifier, "transact") =>
              Select.copy(tree)(transformTerm(qualifier)(owner), resultTransactMethod)
            case _ =>
              super.transformTerm(tree)(owner)
          }
      }

      def processTransactionalMember(tree: Term, name: String): Term = {
        val memberCode: String = tree.show

        if (memberCode.contains(".transact(")) {
          val resultTransactMethod =
            if (memberCode.contains(".run["))
              if (memberCode.contains(".insert("))
                "transactWithMaster"
              else
                "transactWithReplicas"
            else
              report.errorAndAbort("abort", tree.asExprOf[Any])

          new TreeMapper(resultTransactMethod).transformTerm(tree)(tree.symbol)
        } else {
          tree
        }
      }

      tree match {
        case ClassDef(className, constructor, parents, self, classBody) =>
          val newClassBody =
            classBody.map {
              case methodTree@DefDef(methodName, params, returningType, Some(methodBody)) =>
                val methodNewBody = processTransactionalMember(methodBody, s"$className#$methodName")
                DefDef.copy(methodTree)(methodName, params, returningType, Some(methodNewBody))

              case other =>
                other
            }

          val cls = tree.symbol
          val stringMethType = ByNameType.apply(TypeRepr.of[String])
          val stringSym =
            Symbol.newMethod(cls, Symbol.freshName("string"), stringMethType, Flags.Private, Symbol.noSymbol)
          val stringDef = DefDef(stringSym, _ => Some(Literal(StringConstant("macros string"))))

          val toStringMethType = Symbol.requiredMethod("java.lang.Object.toString").info
          val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringMethType, Flags.Override, Symbol.noSymbol)
          val toStringDef = DefDef(toStringOverrideSym, _ => Some(Ref(stringSym)))

          val result =
            List(ClassDef.copy(tree)(className, constructor, parents, self, stringDef :: toStringDef :: newClassBody))

          println("New body classDef: " + result.map(_.show(using Printer.TreeShortCode)))

          result

        case _ =>
          report.error("Only classes supported!", tree.asExprOf[Any])
          List(tree)

      }
    }
  }
}
