package org.hl7.fhir.r4


import java.time.{LocalDate,LocalDateTime}

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}


abstract class MedicationRequest//[S <: Resource: MedicationRequest.Subject]
extends DomainResource
   with Request
   with MedicationRequest.status
   with MedicationRequest.intent
//   with MedicationRequest.subject[S]
{
  this: MedicationRequest.subject[_] =>
}


final object MedicationRequest
extends DomainResourceAttributes
with RequestAttributes
with CanHaveEncounter
with HasMedication
{


  implicit def medicationRequestResourceType[M <: MedicationRequest] =
    Resource.Type[M]("MedicationRequest")


  object Status extends CodedEnum
  {
     type Status = Value

     val Active         = Val("active","Active")
     val OnHold         = Val("on-hold","On Hold")
     val Cancelled      = Val("cancelled","Cancelled")
     val Completed      = Val("completed","Completed")
     val EnteredInError = Val("entered-in-error","Entered in Error")
     val Stopped        = Val("stopped","Stopped")
     val Draft          = Val("draft","Draft")
     val Unknown        = Val("unknown","Unknown")

     implicit val format = json.formatCodedEnum(this)
  }
  type StatusType = Status.Value


  object Intent extends CodedEnum
  {
     type Intent = Value

     val Proposal      = Val("proposal","Proposal")
     val Plan          = Val("plan","Plan")
     val Order         = Val("order","Order")
     val OriginalOrder = Val("original-order","Original Order")
     val ReflexOrder   = Val("reflex-order","Reflex Order")
     val FillerOrder   = Val("filler-order","Filler Order")
     val InstanceOrder = Val("instance-order","Instance Order")
     val Option        = Val("option","option")

     implicit val format = json.formatCodedEnum(this)
  }
  type IntentType = Intent.Value



  trait statusReason[+CC <: CodeableConcept,C[+_]]{
    this: MedicationRequest =>
    val statusReason: C[List[CC]]
  } 
  trait statusReasonNel[+CC <: CodeableConcept]{
    this: MedicationRequest =>
    val statusReason: NonEmptyList[CC]
  } 


  trait supportingInformation[R <: Resource,C[_]]{
    this: MedicationRequest =>
    val supportingInformation: C[List[Reference[R]]]
  }
  trait supportingInformationNel[R <: Resource]{
    this: MedicationRequest =>
    val supportingInformation: NonEmptyList[Reference[R]]
  }


}
