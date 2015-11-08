package models

import java.util.{Calendar, UUID}

import models.Recurrence.Recurrence
import play.api.libs.json._

object Recurrence extends Enumeration {
  type Recurrence = Value
  val ONETIME, WEEKLY = Value
}

case class ClazzDefinition(
                  id: Option[UUID],
                  startFrom: Calendar,
                  endAt: Calendar,
                  activeFrom: Calendar,
                  activeTill: Calendar,
                  recurrence: Recurrence,
                  name: String,
                  contingent: Short,
                  avatarurl: String,
                  description: String,
                  tags: Option[String],
                  idStudio: UUID)


/**
 * The companion object.
 */
object ClazzDefinition {

  import utils.Utils.Implicits._

  /**
   * Converts the [Clazz] object to Json and vice versa.
   */
  implicit val jsonFormat = Json.format[Clazz]
}