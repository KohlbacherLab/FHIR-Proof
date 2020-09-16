package org.hl7.fhir.r4


import java.net.URI

import play.api.libs.json.Json



abstract class Quantity extends Element
{
  val value: Double
}


object Quantity extends ElementAttributes
{

  object Comparator extends Enumeration
  {
    val LessThan           = Value("<")
    val LessThanOrEqual    = Value("<=")
    val GreaterThan        = Value(">")
    val GreaterThanOrEqual = Value(">=")

    implicit val format = Json.formatEnum(this)
  }


  trait unit[C[_]]{
    this: Quantity =>
    val unit: C[String]   //TODO: consider changing from String to closed Unit value set (Enum)?
  }

  trait comparator[C[_]]{
    this: Quantity =>
    val comparator: C[Comparator.Value]
  }

  trait system[C[_]]{
    this: Quantity =>
    val system: C[URI]
  }

  trait code[C[_]]{
    this: Quantity =>
    val code: C[String]   //TODO: consider changing from String to closed Unit value set (Enum)?
  }

}


final case class SimpleQuantity
(
  value: Double,
  unit: Option[String] = None,
  system: Option[URI] = None,
  code: Option[String] = None
)
extends Quantity
   with Quantity.unit[Optional]
   with Quantity.system[Optional]
   with Quantity.code[Optional]

object SimpleQuantity
{
  implicit val format = Json.format[SimpleQuantity]
}


final case class Count private
(
  value: Double,
  code: String,
  system: Option[URI] = None,
)
extends Quantity
   with Quantity.code[Required]
   with Quantity.system[Optional]

object Count
{

  def apply(value: Int): Count =
    Count(value,"1",Some(URI.create("ucum")))


  import play.api.libs.json.{Format,JsPath,Reads}


  implicit val reads  = Reads(_.validate[Int]).map(Count(_))
  implicit val writes = Json.writes[Count]

  implicit val format = Format[Count](reads,writes)

}
