package models.daos


import java.util.UUID

import com.mohiva.play.silhouette.api.LoginInfo
import models._

import scala.concurrent.Future

/**
 * Give access to the partner object.
 */
trait PartnerDAO {

  /**
   * Finds a partner by its login info.
   *
   * @param loginInfo The login info of the partner to find.
   * @return The found partner or None if no partner for the given login info could be found.
   */
  def find(loginInfo: LoginInfo): Future[Option[Partner]]

  /**
   * Saves a partner.
   *
   * @param partner The partner to save.
   * @return The saved partner.
   */
  def save(partner: Partner): Future[Partner]
}
