package org.error.app

import cats.effect.{IO, IOApp, Sync}
import cats.implicits.*
import org.error.macros.TransactorMacros.rewriteDefaultTransactorCalls

import scala.annotation.experimental

class Transactor[F[_]: Sync] {
  def transactWithMaster[A](fa: F[A]): F[A] = {
    fa <* Sync[F].delay(println("Transacted with master"))
  }

  def transactWithReplica[A](fa: F[A]): F[A] = {
    fa <* Sync[F].delay(println("Transacted with replica"))
  }
}

trait TransactorSyntax {
  class MasterTransactionOps[F[_], A](fa: F[A]) {
    def transactWithMaster(xa: Transactor[F]): F[A] =
      xa.transactWithMaster(fa)
  }

  class ReplicasTransactionOps[F[_], A](fa: F[A]) {
    def transactWithReplica(xa: Transactor[F]): F[A] =
      xa.transactWithReplica(fa)
  }

  class DefaultTransactionOps[F[_], A](fa: F[A]) {
    def transact(xa: Transactor[F]): F[A] = {
      println("ERROR! THIS SHOULDN'T BE!")
      xa.transactWithMaster(fa)
    }

    def transactWithMaster(xa: Transactor[F]): F[A] =
      xa.transactWithMaster(fa)

    def transactWithReplica(xa: Transactor[F]): F[A] =
      xa.transactWithReplica(fa)
  }

  implicit def toMasterTransactionOps[F[_], A](fa: F[A]): MasterTransactionOps[F, A] =
    new MasterTransactionOps(fa)

  implicit def toReplicaTransactionOps[F[_], A](fa: F[A]): ReplicasTransactionOps[F, A] =
    new ReplicasTransactionOps(fa)

  implicit def toDefaultTransactionOps[F[_], A](fa: F[A]): DefaultTransactionOps[F, A] =
    new DefaultTransactionOps(fa)

}

object TransactorSyntax extends TransactorSyntax

class Database[F[_]: Sync] {
  def insert(value: String): F[Unit] =
    Sync[F].delay(println(s"inserting value $value in DB"))

  def read(key: String): F[String] =
    Sync[F].delay(println(s"reading key $key from DB")) *> "answer".pure[F]

  def run[A](fa: F[A]): F[A] =
    Sync[F].delay(println(s"running transaction inside DB")) *> fa
}

@experimental @rewriteDefaultTransactorCalls
class DbService[F[_]: Sync](database: Database[F], transactor: Transactor[F]) extends TransactorSyntax {
  def doAction: F[Unit] =
    database.run(database.insert("ValueToInsert")).transact(transactor)

  def doRead: F[String] =
    database.run(database.read("SomeKey")).transact(transactor)
}

@experimental
object Main extends IOApp.Simple {

  override def run: IO[Unit] = {
    val database = new Database[IO]
    val transactor = new Transactor[IO]
    val service = new DbService[IO](database, transactor)

    IO.delay(println("Is this even working? " + service.toString)) *> service.doAction *> service.doRead.void
  }
}