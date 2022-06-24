package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:, CNil}


abstract class ClaimResponse
extends DomainResource
   with Event
{

  this: ClaimResponse.created[_] =>

  val status: Claim.Status.Value

  val `type`: CodeableConcept with CodeableConcept.codingNel[CodingStatic[Claim.Type.Value]]

  val use: Claim.Use.Value

  val patient: Reference[Patient]

  val insurer: Reference[Organization]

  val outcome: ClaimResponse.Outcome.Value

}


final object ClaimResponse
extends DomainResourceAttributes
with EventAttributes[Patient :+: CNil]
{


  implicit def claimResourceType[R <: ClaimResponse] =
    Resource.Type[R]("ClaimResponse")


  object Outcome extends Enumeration
  {
     type Outcome = Value

     val Queued   = Value("queued")
     val Complete = Value("complete")
     val Error    = Value("error")
     val Partial  = Value("partial")

     implicit val system =
       CodingSystem[Outcome]("http://hl7.org/fhir/remittance-outcome")

     implicit val format = Json.formatEnum(this)
  }


  trait created[T <: Temporal]{
    this: ClaimResponse =>
    val created: T
  }

  trait requestor[R <: DomainResource,C[+_]]{
    this: ClaimResponse =>
    val requestor: C[Reference[R]]
  }

  trait request[C[+_]]{
    this: ClaimResponse =>
    val request: C[Reference[Claim]]
  }



}
