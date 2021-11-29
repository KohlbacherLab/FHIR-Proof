package org.hl7.fhir.r4



import java.time.Instant
import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}


sealed abstract class DiagnosticReport
extends DomainResource
with Event
with HasStatus[DiagnosticReport.Status.Value]


abstract class DiagnosticReportSC
extends DiagnosticReport
   with HasStaticCode


abstract class DiagnosticReportDC[S]
extends DiagnosticReport
   with HasCode[S]


final object DiagnosticReport
extends DomainResourceAttributes
with EventAttributes[Patient :+: Group :+: Device :+: CNil]
with CanHaveBasedOn
with CanHaveEncounter
with CanHaveEffective[Temporal :+: Period[_] :+: CNil]
{


  implicit def diagnosticReportResourceType[C <: DiagnosticReport] =
    Resource.Type[C]("DiagnosticReport")


  object Status extends CodedEnum
  {
    val Registered  = Val("registered","Registered")
    val Preliminary = Val("preliminary","Preliminary")
    val Final       = Val("final", "Final")
    val Amended     = Val("amended","Amended")
    //TODO: further values

    implicit val format = json.formatCodedEnum(this)
  }
  type StatusType = Status.Value


  trait issued[T <: Temporal, C[_]]{
    this: DiagnosticReport =>
    val issued: C[T]
  }

  trait specimen[+S <: Specimen, C[+_]]{
    this: DiagnosticReport =>
    val specimen: C[List[Reference[S]]]
  }
  trait specimenNel[+S <: Specimen]{
    this: DiagnosticReport =>
    val specimen: NonEmptyList[Reference[S]]
  }


  trait performer[+P <: DomainResource, C[+_]]{
    this: DiagnosticReport =>
    val performer: C[List[Reference[P]]]
  }
  trait performerNel[+P <: DomainResource]{
    this: DiagnosticReport =>
    val performer: NonEmptyList[Reference[P]]
  }

  trait result[+O <: Observation, C[+_]]{
    this: DiagnosticReport =>
    val result: C[List[Reference[O]]]
  }
  trait resultNel[+O <: Observation]{
    this: DiagnosticReport =>
    val result: NonEmptyList[Reference[O]]
  }


  trait conclusion[C[+_]]{
    this: DiagnosticReport =>
    val conclusion: C[String]
  }

  trait conclusionCode[+CC <: CodeableConcept,C[+_]]{
    this: DiagnosticReport =>
    val conclusionCode: C[List[CC]]
  }


  trait presentedForm[+A <: Attachment, C[+_]]{
    this: DiagnosticReport =>
    val presentedForm: C[List[A]]
  }
  trait presentedFormNel[+A <: Attachment]{
    this: DiagnosticReport =>
    val presentedForm: NonEmptyList[A]
  }

}
