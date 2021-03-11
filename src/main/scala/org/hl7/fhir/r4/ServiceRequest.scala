package org.hl7.fhir.r4


import java.time.{LocalDate,LocalDateTime}

import cats.data.NonEmptyList

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


  object Status extends CodedEnum
  {
     type Status = Value

     val Draft          = Val("draft","Draft")
     val Active         = Val("active","Active")
     val OnHold         = Val("on-hold","On Hold")
     val Revoked        = Val("revoked","Revoked")
     val Completed      = Val("completed","Completed")
     val EnteredInError = Val("entered-in-error","Entered in Error")
     val Unknown        = Val("unknown","Unknown")

     implicit val format = json.formatCodedEnum(this)
  }
  type StatusType = Status.Value


  object Intent extends CodedEnum
  {
     type Intent = Value

     val Proposal      = Val("proposal","Proposal")
     val Plan          = Val("plan","Plan")
     val Directive     = Val("directive","Directive")
     val Order         = Val("order","Order")
     val OriginalOrder = Val("original-order","Original Order")
     val ReflexOrder   = Val("reflex-order","Reflex Order")
     val FillerOrder   = Val("filler-order","Filler Order")
     val InstanceOrder = Val("instance-order","Instance Order")
     val Option        = Val("option","option")

     implicit val format = json.formatCodedEnum(this)
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


  trait specimen[+S <: Specimen, C[+_]]{
    this: ServiceRequest =>
    val specimen: C[List[Reference[S]]]
  }
  trait specimenNel[+S <: Specimen]{
    this: ServiceRequest =>
    val specimen: NonEmptyList[Reference[S]]
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
