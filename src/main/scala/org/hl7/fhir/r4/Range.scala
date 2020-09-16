package org.hl7.fhir.r4


import java.net.URI

import play.api.libs.json.Json



abstract class Range extends Element
{
  def contains(v: Double): Boolean
}


object Range extends ElementAttributes
{

  trait high[+Q <: Quantity,C[+_]]{
    this: Range =>
    val high: C[Q]
  }

  trait low[+Q <: Quantity,C[+_]]{
    this: Range =>
    val low: C[Q]
  }

}


final case class BasicRange
(
  low: SimpleQuantity,
  high: SimpleQuantity
)
extends Range
   with Range.low[SimpleQuantity,Required]
   with Range.high[SimpleQuantity,Required]
{
  def contains(v: Double): Boolean =
    low.value <= v && v <= high.value
}

object BasicRange
{

  def apply(low: Double, high: Double): BasicRange =
    BasicRange(SimpleQuantity(low), SimpleQuantity(high))

  implicit val format = Json.format[BasicRange]
}


final case class LBoundedRange
(
  low: SimpleQuantity,
  high: Option[SimpleQuantity]
)
extends Range
   with Range.low[SimpleQuantity,Required]
   with Range.high[SimpleQuantity,Optional]
{

  def contains(v: Double): Boolean =
    low.value <= v
}

object LBoundedRange
{

  def apply(low: Double, high: Option[Double]): LBoundedRange =
    LBoundedRange(SimpleQuantity(low), high.map(SimpleQuantity(_)))

  implicit val format = Json.format[LBoundedRange]
}


