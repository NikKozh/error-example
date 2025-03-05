Goal: macros that rewrite every `.transact` call in class into `.transactWithMaster` or `.transactWithReplica` calls according to method body content.
Problem: on build stage I see correct rewrited methods, but in runtime app still using default transact calls.

Build output:

`New body classDef:`
```scala
 List(@SourceFile("app/src/main/scala/org/error/app/Main.scala") @rewriteDefaultTransactorCalls @experimental class DbService[F[_$9]](database: Database[F], transactor: Transactor[F])(using evidence$1: Sync[F]) extends TransactorSyntax {
  def string$macro$1: String = "macros string"
  override def toString(): String = DbService.this.string$macro$1
  def doAction: F[Unit] = DbService.this.toDefaultTransactionOps[F, Unit](database.run[Unit](database.insert("ValueToInsert"))).transactWithMaster(transactor)
  def doRead: F[String] = DbService.this.toDefaultTransactionOps[F, String](database.run[String](database.read("SomeKey"))).transactWithReplicas(transactor)
})
```
(new transact methods are here)

Runtime output:
```
ERROR! THIS SHOULDN'T BE!
ERROR! THIS SHOULDN'T BE!
Is this even working? macros string
running transaction inside DB
inserting value ValueToInsert in DB
Transacted with master
running transaction inside DB
reading key SomeKey from DB
Transacted with master
```

(`.toString` macro part is here, so macros changes _was_ applied, but new transact call with replica is not)
