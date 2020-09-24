package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}


abstract class ClaimResponse
extends DomainResource
   with Event
{

  this: ClaimResponse.created[_] =>

  val status: Claim.Status.Value

  val `type`: CodeableConcept with CodeableConcept.codingNel[Coding[Claim.Type.Value]]

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


  object Outcome extends CodedEnum
  {
     type Outcome = Value

     val Queued   = Val("queued"  ,"Queued")
     val Complete = Val("complete","Processing Complete")
     val Error    = Val("error"   ,"Error")
     val Partial  = Val("partial" ,"Partial Processing")

     implicit val system =
       Coding.System[Outcome]("http://hl7.org/fhir/remittance-outcome")

     implicit val format = json.formatCodedEnum(this)
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
