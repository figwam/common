package models.daos

import java.sql.Timestamp
import javax.inject.Inject


import models.{Address, AppLogger}
import play.api.db.slick.DatabaseConfigProvider

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


trait AddressDAO  {

  def update(address: Address): Future[Address]

}

class AddressDAOImpl @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
  extends AddressDAO with DAOSlick {

  import driver.api._


  override def update(address: Address): Future[Address] = {
    val q = for {a <- slickAddresses if a.id === address.id}yield (a.street, a.city, a.zip, a.updatedOn)
    val updateAction = q.update(address.street, address.city, address.zip, new Timestamp(System.currentTimeMillis))
    db.run(updateAction).map(_ => address)
     }

}
