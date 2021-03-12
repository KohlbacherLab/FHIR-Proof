package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}


abstract class Claim
extends DomainResource
   with Request
   with Claim.status
{

  this: Claim.created[_] with Claim.provider[_] =>

  val `type`: CodeableConcept with CodeableConcept.codingNel[Coding[Claim.Type.Value]]

  val use: Claim.Use.Value

  val priority: CodeableConcept with CodeableConcept.codingNel[Coding[ProcessPriority.Value]]

  val patient: Reference[Patient]

}


final object Claim
extends DomainResourceAttributes
with RequestAttributes
{


  implicit def claimResourceType[R <: Claim] =
    Resource.Type[R]("Claim")


  object Status extends CodedEnum
  {
     type Status = Value

     val Active         = Val("active"          ,"Active")
     val Cancelled      = Val("cancelled"       ,"Cancelled")
     val Draft          = Val("draft"           ,"Draft")
     val EnteredInError = Val("entered-in-error","Entered in Error")

     implicit val format = json.formatCodedEnum(this)
  }
  type StatusType = Status.Value


  object Type extends CodedEnum
  {
     type Type = Value

     val Institutional = Val("institutional","Institutional")
     val Oral          = Val("oral"         ,"Oral")
     val Pharmacy      = Val("pharmacy"     ,"Pharmacy")
     val Professional  = Val("professional" ,"Professional")
     val Vision        = Val("vision"       ,"Vision")

     implicit val system =
       Coding.System[Value]("http://terminology.hl7.org/CodeSystem/claim-type")

     implicit val format = json.formatCodedEnum(this)
  }


  object Use extends CodedEnum
  {
     type Use = Value

     val Claim            = Val("claim"           , "Claim")
     val Preauthorization = Val("preauthorization", "Preauthorization")
     val Predetermination = Val("predetermination", "Predetermination")

     implicit val format = json.formatCodedEnum(this)
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
