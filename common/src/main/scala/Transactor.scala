package org.error.common

trait Transactor[F[_]] {
  def transactWithMaster[A](fa: F[A]): F[A]

  def transactWithReplica[A](fa: F[A]): F[A]
}
