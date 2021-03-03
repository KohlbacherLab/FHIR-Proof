package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList


abstract class Consent
extends DomainResource
with HasStatus[Consent.Status.Value]
{
  val scope: CodeableConcept with CodeableConcept.codingNel[Coding[Consent.Scope.Value]]
  val category: NonEmptyList[CodeableConcept with CodeableConcept.codingNel[Coding[_]]]
}


object Consent
extends DomainResourceAttributes
{

  implicit def consentResourceType[C <: Consent] =
    Resource.Type[C]("Consent")


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

/*
  trait category[S]{
    this: Consent =>
    val category: NonEmptyList[CodeableConcept with CodeableConcept.codingNel[Coding[S]]]
  }
*/

  trait patient[C[+_]]{
    this: Consent =>
    val patient: C[Reference[Patient]]
  }

  trait dateTime[T <: Temporal, C[_]]{
    this: Consent =>
    val dateTime: C[T]
  }






}
