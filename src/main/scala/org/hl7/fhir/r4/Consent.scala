package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json


abstract class Consent
extends DomainResource
with HasStatus[Consent.Status.Value]
{
  val scope: CodeableConcept with CodeableConcept.codingNel[CodingStatic[Consent.Scope.Value]]
  val category: NonEmptyList[CodeableConcept with CodeableConcept.codingNel[Coding]]
}


object Consent
extends DomainResourceAttributes
{

  implicit def consentResourceType[C <: Consent] =
    Resource.Type[C]("Consent")


  object Status extends Enumeration
  {
    val Draft          = Value("draft")
    val Proposed       = Value("proposed")
    val Active         = Value("active")
    val Rejected       = Value("rejected")
    val Inactive       = Value("inactive")
    val EnteredInError = Value("entered-in-error")

    implicit val format = Json.formatEnum(this)
  }


  object Scope extends Enumeration
  {
    val Adr            = Value("adr")
    val Research       = Value("research")
    val PatientPrivacy = Value("patient-privacy")
    val Treatment      = Value("treatment")

    implicit val system =
      CodingSystem[Scope.Value]("http://terminology.hl7.org/CodeSystem/consentscope")

    implicit val format = Json.formatEnum(this)
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
