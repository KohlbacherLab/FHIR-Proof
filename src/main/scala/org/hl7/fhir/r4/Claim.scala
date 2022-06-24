package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:, CNil}


abstract class Claim
extends DomainResource
   with Request
   with Claim.status
{

  this: Claim.created[_] with Claim.provider[_] =>

  val `type`: CodeableConcept with CodeableConcept.codingNel[CodingStatic[Claim.Type.Value]]

  val use: Claim.Use.Value

  val priority: CodeableConcept with CodeableConcept.codingNel[CodingStatic[ProcessPriority.Value]]

  val patient: Reference[Patient]

}


final object Claim
extends DomainResourceAttributes
with RequestAttributes
{


  implicit def claimResourceType[R <: Claim] =
    Resource.Type[R]("Claim")


  object Status extends Enumeration
  {
     type Status = Value

     val Active         = Value("active")
     val Cancelled      = Value("cancelled")
     val Draft          = Value("draft")
     val EnteredInError = Value("entered-in-error")

     implicit val format = Json.formatEnum(this)
  }
  type StatusType = Status.Value


  object Type extends Enumeration
  {
     type Type = Value

     val Institutional = Value("institutional")
     val Oral          = Value("oral")
     val Pharmacy      = Value("pharmacy")
     val Professional  = Value("professional")
     val Vision        = Value("vision")

     implicit val system =
       CodingSystem[Value]("http://terminology.hl7.org/CodeSystem/claim-type")

     implicit val format = Json.formatEnum(this)
  }


  object Use extends Enumeration
  {
     type Use = Value

     val Claim            = Value("claim")
     val Preauthorization = Value("preauthorization")
     val Predetermination = Value("predetermination")

     implicit val format = Json.formatEnum(this)
  }


  trait created[T <: Temporal]{
    this: Claim =>
    val created: T
  }


  trait prescription[R <: Resource with Request,C[+_]]{
    this: Claim =>
    val prescription: C[Reference[R]]
  }


  trait provider[R <: DomainResource]{
    this: Claim =>
    val provider: Reference[R]
  }


  trait supportingInfo[R <: Resource,C[+_]]{
    this: Claim =>
    val supportingInfo: C[List[Reference[R]]]
  }
  trait supportingInfoNel[R <: Resource]{
    this: Claim =>
    val supportingInfo: NonEmptyList[Reference[R]]
  }


}
