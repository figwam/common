package models.daos

import java.sql.Timestamp
import java.util.UUID

import com.mohiva.play.silhouette.api.LoginInfo
import models._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import javax.inject.Inject
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.Future


import utils.Utils.asCalendar

/**
 * Give access to the partner object using Slick
 */
class PartnerDAOImpl @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)
  extends PartnerDAO with DAOSlick {

  import driver.api._

  /**
   * Finds a partner by its login info.
   *
   * @param loginInfo The login info of the partner to find.
   * @return The found partner or None if no partner for the given login info could be found.
   */
  def find(loginInfo: LoginInfo) = {
    val query = for {
      dbLoginInfo <- loginInfoQuery(loginInfo)
      dbPartnerLoginInfo <- slickPartnerLoginInfos.filter(_.idLoginInfo === dbLoginInfo.id)
      dbPartner <- slickPartners.filter(_.id === dbPartnerLoginInfo.idPartner)
      dbAddress <- slickAddresses.filter(_.id === dbPartner.idAddress)
    } yield (dbPartner, dbLoginInfo, dbAddress)
    db.run(query.result.headOption).map { resultOption =>
      resultOption.map {
        case (partner, loginInfo, address) =>
          Partner(partner.id,
            LoginInfo(loginInfo.providerId, loginInfo.providerKey),
            partner.firstname,
            partner.lastname,
            partner.mobile,
            partner.phone,
            partner.email,
            partner.emailVerified,
            partner.createdOn,
            partner.updatedOn,
            partner.ptoken,
            partner.isActive,
            partner.inactiveReason,
            partner.username,
            partner.fullname,
            partner.avatarurl,
            Address(
              address.id,
              address.street,
              address.city,
              address.zip,
              address.state,
              address.country)
          )
      }
    }
  }

  /**
   * Saves a partner.
   *
   * @param partner The partner to save.
   * @return The saved partner.
   */
  def save(partner: Partner) = {
    val dbPartner = DBPartner(
      partner.id,
      partner.firstname,
      partner.lastname,
      partner.mobile,
      partner.phone,
      partner.email,
      false,
      new Timestamp(System.currentTimeMillis),
      new Timestamp(System.currentTimeMillis),
      partner.ptoken,
      false,
      None,
      true,
      partner.inactiveReason,
      UUID.randomUUID(), // Will be set later
      partner.username,
      partner.fullname,
      partner.avatarurl)

    val dbAddress = DBAddress(
      None,
      partner.address.street,
      partner.address.zip,
      partner.address.city,
      partner.address.state,
      partner.address.country,
      new Timestamp(System.currentTimeMillis),
      new Timestamp(System.currentTimeMillis), false, None, None)

    val dbLoginInfo = DBLoginInfo(None,
      partner.loginInfo.providerID,
      partner.loginInfo.providerKey,
      new Timestamp(System.currentTimeMillis),
      new Timestamp(System.currentTimeMillis),
      None,
      new Timestamp(System.currentTimeMillis))

    // We don't have the address id so we try to get it first.
    // If there is no Address yet for this partner we retrieve the id on insertion.
    val addressAction = {
      val retrieveAddress = slickAddresses.filter(
        address => address.id === partner.address.id).result.headOption
      val insertAddress = slickAddresses.returning(slickAddresses.map(_.id)).
        into((address, id) => address.copy(id = id)) += dbAddress
      for {
        addressOption <- retrieveAddress
        address <- addressOption.map(DBIO.successful(_)).getOrElse(insertAddress)
      } yield address
    }

    // We don't have the LoginInfo id so we try to get it first.
    // If there is no LoginInfo yet for this partner we retrieve the id on insertion.
    val loginInfoAction = {
      val retrieveLoginInfo = slickLoginInfos.filter(
        info => info.providerId === partner.loginInfo.providerID && info.providerKey === partner.loginInfo.providerKey).result.headOption
      val insertLoginInfo = slickLoginInfos.returning(slickLoginInfos.map(_.id)).
        into((info, id) => info.copy(id = id)) += dbLoginInfo
      for {
        loginInfoOption <- retrieveLoginInfo
        loginInfo <- loginInfoOption.map(DBIO.successful(_)).getOrElse(insertLoginInfo)
      } yield loginInfo
    }

    // combine database actions to be run sequentially
    val actions = (for {
      address <- addressAction
      dbPartnerP <- slickPartners.returning(slickPartners.map(_.id)).insertOrUpdate(dbPartner.copy(idAddress = address.id.get))
      loginInfo <- loginInfoAction
      _ <- slickPartnerLoginInfos += DBPartnerLoginInfo(new Timestamp(System.currentTimeMillis), dbPartnerP.head.get, loginInfo.id.get)
    } yield ()).transactionally
    // run actions and return partner afterwards
    db.run(actions).map(_ => partner)
  }
}
