package org.hl7.fhir.r4


import java.time.{LocalDate,LocalDateTime}

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:, CNil}


abstract class ServiceRequest
extends DomainResource
   with Request
   with ServiceRequest.status
   with ServiceRequest.intent
{
  this: ServiceRequest.subject[_] =>
}


final object ServiceRequest
extends DomainResourceAttributes
with RequestAttributes
with CanHaveEncounter
with CanHaveNotes
{


  implicit def serviceRequestResourceType[M <: ServiceRequest] =
    Resource.Type[M]("ServiceRequest")


  object Status extends Enumeration
  {
     type Status = Value

     val Draft          = Value("draft")
     val Active         = Value("active")
     val OnHold         = Value("on-hold")
     val Revoked        = Value("revoked")
     val Completed      = Value("completed")
     val EnteredInError = Value("entered-in-error")
     val Unknown        = Value("unknown")

     implicit val format = Json.formatEnum(this)
  }
  type StatusType = Status.Value


  object Intent extends Enumeration
  {
     type Intent = Value

     val Proposal      = Value("proposal")
     val Plan          = Value("plan")
     val Directive     = Value("directive")
     val Order         = Value("order")
     val OriginalOrder = Value("original-order")
     val ReflexOrder   = Value("reflex-order")
     val FillerOrder   = Value("filler-order")
     val InstanceOrder = Value("instance-order")
     val Option        = Value("option")

     implicit val format = Json.formatEnum(this)
  }
  type IntentType = Intent.Value


  trait category[+CC <: CodeableConcept, F[+_]]{
    this: ServiceRequest =>
    val category: F[List[CC]]
  }
  trait categoryNel[+CC <: CodeableConcept]{
    this: ServiceRequest =>
    val category: NonEmptyList[CC]
  }


  trait code[+CC <: CodeableConcept,C[+_]]{
    this: ServiceRequest =>
    val code: C[CC]
  }


  trait specimen[C[+_]]{
    this: ServiceRequest =>
    val specimen: C[List[Reference[Specimen]]]
  }
  trait specimenNel{
    this: ServiceRequest =>
    val specimen: NonEmptyList[Reference[Specimen]]
  }


  trait supportingInfo[R <: Resource,C[_]]{
    this: Request =>
    val supportingInfo: C[List[Reference[R]]]
  }
  trait supportingInfoNel[R <: Resource]{
    this: Request =>
    val supportingInfo: NonEmptyList[Reference[R]]
  }


}
