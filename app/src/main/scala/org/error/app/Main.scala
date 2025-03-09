package org.error.app

import cats.effect.{IO, IOApp, Sync}
import cats.implicits.*
import org.error.common.Transactor
import org.error.macros.TransactorMacros.transactional
import scala.language.implicitConversions

class TransactorImpl[F[_]: Sync] extends Transactor[F] {
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
      println("ERROR! THERE IS SHOULDN'T BE DEFAULT TRANSACTION CALLS")
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


class DbService[F[_]: Sync](database: Database[F], transactor: Transactor[F]) extends TransactorSyntax {
  def doAction: F[Unit] =
    transactional { database.run(database.insert("ValueToInsert")) }(transactor)//.transact(transactor)

  def doRead: F[String] =
    transactional {database.run(database.read("SomeKey")) }(transactor)
}


object Main extends IOApp.Simple {

  override def run: IO[Unit] = {
    val database = new Database[IO]
    val transactor = new TransactorImpl[IO]
    val service = new DbService[IO](database, transactor)

    IO.delay(println("service.toString: " + service.toString)) *> service.doAction *> service.doRead.void
  }
}