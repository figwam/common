package models.daos


import java.util.UUID

import com.mohiva.play.silhouette.api.LoginInfo
import models._

import scala.concurrent.Future


trait RegistrationDAO {

  def countByTrainee(idTrainee:UUID): Future[Int]

  def save(registration: Registration): Future[Registration]

  def delete(idRegistration: UUID): Future[Int]
}
