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
  extension [F[_], A] (inline fa: F[A])
    inline def transact(xa: Transactor[F]): F[A] = {
      transactional { fa }(xa)
    }
}

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
    database.run(database.insert("ValueToInsert")).transact(transactor)

  def doRead: F[String] =
    database.run(database.read("SomeKey")).transact(transactor)
}


object Main extends IOApp.Simple {

  override def run: IO[Unit] = {
    val database = new Database[IO]
    val transactor = new TransactorImpl[IO]
    val service = new DbService[IO](database, transactor)

    IO.delay(println("service.toString: " + service.toString)) *> service.doAction *> service.doRead.void
  }
}