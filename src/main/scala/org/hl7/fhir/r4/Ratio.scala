package org.hl7.fhir.r4


import java.net.URI

import play.api.libs.json.Json

import shapeless.{Coproduct, :+:, CNil}



abstract class Ratio extends Element

object Ratio extends ElementAttributes
{

  trait denominator[+Q <: Quantity,C[+_]]{
    this: Ratio =>
    val denominator: C[Q]
  }

  trait numerator[+Q <: Quantity,C[+_]]{
    this: Ratio =>
    val numerator: C[Q]
  }

}


final case class BasicRatio
(
  numerator: SimpleQuantity,
  denominator: SimpleQuantity
)
extends Ratio
   with Ratio.numerator[SimpleQuantity,Required]
   with Ratio.denominator[SimpleQuantity,Required]

object BasicRatio
{
  implicit val format = Json.format[BasicRatio]
}



/*
final class Ratio[
  Num: Ratio.Valid,
  Denom: Ratio.Valid
](
  val numerator: Num
  val denominator: Denom
)(
  implicit eq: Num =:= Denom,
)
extends Element

object Ratio extends ElementAttributes
{

  type Valid[T] = T IsIn (Quantity :+: Option[_ <: Quantity] :+: CNil)

  trait denominator[+Q <: Quantity,C[+_]]{
    this: Ratio =>
    val denominator: C[Q]
  }

  trait numerator[+Q <: Quantity,C[+_]]{
    this: Ratio =>
    val numerator: C[Q]
  }

}
*/

