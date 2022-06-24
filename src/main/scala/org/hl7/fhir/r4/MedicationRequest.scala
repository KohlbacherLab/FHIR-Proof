package org.hl7.fhir.r4


import java.time.{LocalDate,LocalDateTime}

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:, CNil}


abstract class MedicationRequest
extends DomainResource
   with Request
   with MedicationRequest.status
   with MedicationRequest.intent
{
  this: MedicationRequest.subject[_] =>
}


final object MedicationRequest
extends DomainResourceAttributes
with RequestAttributes
with CanHaveEncounter
with CanHaveReason[MedicationRequest]
with HasMedication
{


  implicit def medicationRequestResourceType[M <: MedicationRequest] =
    Resource.Type[M]("MedicationRequest")


  object Status extends Enumeration
  {
     type Status = Value

     val Active         = Value("active")
     val OnHold         = Value("on-hold")
     val Cancelled      = Value("cancelled")
     val Completed      = Value("completed")
     val EnteredInError = Value("entered-in-error")
     val Stopped        = Value("stopped")
     val Draft          = Value("draft")
     val Unknown        = Value("unknown")

     implicit val format = Json.formatEnum(this)
  }
  type StatusType = Status.Value


  object Intent extends Enumeration
  {
     type Intent = Value

     val Proposal      = Value("proposal")
     val Plan          = Value("plan")
     val Order         = Value("order")
     val OriginalOrder = Value("original-order")
     val ReflexOrder   = Value("reflex-order")
     val FillerOrder   = Value("filler-order")
     val InstanceOrder = Value("instance-order")
     val Option        = Value("option")

     implicit val format = Json.formatEnum(this)
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
