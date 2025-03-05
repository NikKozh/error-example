package org.error.macros

import scala.annotation.{MacroAnnotation, experimental}
import scala.quoted.*

object TransactorMacros {

  // Методы детектируем обычным поиском по тексту исходного кода, поэтому учитываем различный вид их вызова
  private def withBraces(str: String) = Seq(str + "(", str + " {", str + "[")

  private val WRITE_METHODS = Seq(
    ".insertValue",
    ".insert",
    ".updateValue",
    ".update"
  ).flatMap(withBraces) :+ ".delete"

  // `stream` тоже считается "run"-методом, т.к. можно вызвать `stream(...).transact(transactor)`
  private val RUN_METHODS = Seq("run", "stream").flatMap(withBraces)

  private val TRANSACT_METHOD = "transact"
  private val MASTER_TRANSACT_METHOD = "transactWithMaster"
  private val REPLICAS_TRANSACT_METHOD = "transactWithReplicas"

  @experimental
  class rewriteDefaultTransactorCalls extends MacroAnnotation {

    override def transform(using quotes: Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = {
      import quotes.reflect.*

      /** Обходит AST (абстрактное синтаксическое дерево, типизированное представление Scala-кода), переписывая метод
       * `.transact`
       *
       * @param resultTransactMethod
       * новый метод запуска транзакции, на который необходимо переписать дефолтный
       */
      class TreeMapper(resultTransactMethod: String) extends TreeMap {
        /** Метод трансформации
         *
         * @param tree
         * исходное AST члена класса
         * @param owner
         * владелец исходного дерева
         * @return
         * копия AST с изменённым вызовом `.transact`
         */
        override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term =
          tree match {
            case s@Select(qualifier, TRANSACT_METHOD) =>
//              println(s"- REAL method rewriting on $resultTransactMethod: " + s.show(using Printer.TreeStructure))
              Select.copy(tree)(transformTerm(qualifier)(owner), resultTransactMethod)
//            case s@Select(qualifier, "toDefaultTransactionOps") =>
//              println(s"- REAL ops rewriting on $resultTransactMethod")
//              Select.copy(tree)(transformTerm(qualifier)(owner), "toReplicasTransactionOps")
            case _ =>
              super.transformTerm(tree)(owner)
          }
      }

      /** Преобразовывает член класса, в котором может быть вызов транзакции
       *
       * @param tree
       * AST метода или val
       * @param name
       * имя члена-класса для понятного лога в ходе билда
       * @return
       * переписанное AST
       */
      def processTransactionalMember(tree: Term, name: String): Term = {
        // Преобразовываем AST в обычную строку, содержащую по сути текст исходного кода для этого члена класса
        val memberCode: String = tree.show

        // Ищем вызов транзакции простым поиском по строке в исходном коде
        if (withBraces(s".$TRANSACT_METHOD").exists(memberCode.contains)) {
          val resultTransactMethod =
            if (RUN_METHODS.exists(memberCode.contains))
              if (WRITE_METHODS.exists(memberCode.contains))
                MASTER_TRANSACT_METHOD
              else
                REPLICAS_TRANSACT_METHOD
            else
              report.errorAndAbort(
                s"""Class member `$name` has ``.$TRANSACT_METHOD` call, but no `.run` or `.stream`!
                   |Since there is no information about SQL operations, you have to choose appropriate method manually:
                   |`$MASTER_TRANSACT_METHOD` if there is SQL actions (insert, update, delete)
                   |`$REPLICAS_TRANSACT_METHOD` if there is ONLY read operations""".stripMargin,
                tree.asExprOf[Any]
              )

          // Логирование происходит на стадии билда, как у Quill с выводом текста SQL-запросов
//          report.info(s"Member `$name` new transact call: " + resultTransactMethod, tree.asExprOf[Any])

          new TreeMapper(resultTransactMethod).transformTerm(tree)(tree.symbol)
        } else if (memberCode.contains(s".$REPLICAS_TRANSACT_METHOD") && WRITE_METHODS.exists(memberCode.contains)) {
          report.errorAndAbort(
            s"""Class member `$name` has SQL actions (insert, update and/or delete), but `.$REPLICAS_TRANSACT_METHOD`
               |transaction call was detected. Change it to `$MASTER_TRANSACT_METHOD` call.""".stripMargin,
            tree.asExprOf[Any]
          )
        } else {
          tree
        }
      }

      // Смотрим, куда навесили макрос
      tree match {
        // Если это класс - начинаем его анализировать
        case ClassDef(className, constructor, parents, self, classBody) =>
          val newClassBody =
            classBody.map {
              // Переписываем тело каждого найденного метода
              case methodTree@DefDef(methodName, params, returningType, Some(methodBody)) =>
//                println(s"old body for $className#$methodName: ${methodBody.show(using Printer.TreeStructure)}")
                val methodNewBody = processTransactionalMember(methodBody, s"$className#$methodName")
//                println(s"new body for $className#$methodName: ${methodNewBody.show(using Printer.TreeShortCode)}")
                DefDef.copy(methodTree)(methodName, params, returningType, Some(methodNewBody))

              // Или тело val
              case valTree@ValDef(valName, returningType, Some(valBody)) =>
                //                println(s"old body for $className#$valName: ${valBody.show(using Printer.TreeShortCode)}")
                val valNewBody = processTransactionalMember(valBody, s"$className#$valName")
//                println(s"new body for $className#$valName: ${valNewBody.show(using Printer.TreeShortCode)}")
                ValDef.copy(valTree)(valName, returningType, Some(valNewBody))

              // Всё остальное содержимое класса оставляем без изменений
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

          val x =
            List(ClassDef.copy(tree)(className, constructor, parents, self, stringDef :: toStringDef :: newClassBody))
          println("New body classDef: " + x.map(_.show(using Printer.TreeShortCode)))

          x

        case _ =>
          report.error("Only classes supported!", tree.asExprOf[Any])
          List(tree)

      }
    }
  }
}
