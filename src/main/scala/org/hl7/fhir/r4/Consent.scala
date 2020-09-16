package org.hl7.fhir.r4



import java.time.temporal.Temporal



abstract class Consent
extends DomainResource
with HasStatus[Consent.Status.Value]
{
  val scope: CodeableConcept with CodeableConcept.codingNel[Coding[Consent.Scope.Value]]
}


object Consent
extends DomainResourceAttributes
{


  object Status extends CodedEnum
  {
    val Draft          = Val("draft", "Draft")
    val Proposed       = Val("proposed","Proposed")
    val Active         = Val("active", "Active")
    val Rejected       = Val("rejected", "Rejected")
    val Inactive       = Val("inactive", "Inactive")
    val EnteredInError = Val("entered-in-error", "Entered in Error")

    implicit val format = json.formatCodedEnum(this)
  }


  object Scope extends CodedEnum
  {
    val Adr            = Val("adr","Advanced Care Directive")
    val Research       = Val("research","Research")
    val PatientPrivacy = Val("patient-privacy","Privacy Consent")
    val Treatment      = Val("treatment","Treatment")

    implicit val system = Coding.System[Scope.Value]("http://terminology.hl7.org/CodeSystem/consentscope")

    implicit val format = json.formatCodedEnum(this)
  }


  trait patient[C[+_]]{
    this: Consent =>
    val patient: C[Reference[Patient]]
  }

  trait dateTime[T <: Temporal, C[_]]{
    this: Consent =>
    val dateTime: C[T]
  }







}
