package models.daos


import java.util.UUID

import com.mohiva.play.silhouette.api.LoginInfo
import models._

import scala.concurrent.Future

/**
 * Give access to the trainee object.
 */
trait TraineeDAO {

  /**
   * Finds a trainee by its login info.
   *
   * @param loginInfo The login info of the trainee to find.
   * @return The found trainee or None if no trainee for the given login info could be found.
   */
  def find(loginInfo: LoginInfo): Future[Option[Trainee]]

  /**
   * Saves a trainee.
   *
   * @param trainee The trainee to save.
   * @return The saved trainee.
   */
  def save(trainee: Trainee): Future[Trainee]
}
