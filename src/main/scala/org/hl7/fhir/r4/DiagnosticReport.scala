package org.hl7.fhir.r4



import java.time.Instant
import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}



abstract class DiagnosticReport
extends DomainResource
with Event
with HasStatus[DiagnosticReport.Status.Value]
with HasLOINCCode


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
/*
  trait issued[C[_]]{
    this: DiagnosticReport =>
    val issued: C[Instant]
  }
*/

  trait specimen[+S <: Specimen, C[+_]]{
    this: DiagnosticReport =>
    val specimen: C[List[Reference[S]]]
  }
  trait specimenNel[+S <: Specimen]{
    this: DiagnosticReport =>
    val specimen: NonEmptyList[Reference[S]]
  }


  trait result[+O <: Observation, C[+_]]{
    this: DiagnosticReport =>
    val result: C[List[Reference[O]]]
  }
  trait resultNel[+O <: Observation]{
    this: DiagnosticReport =>
    val result: NonEmptyList[Reference[O]]
  }



}
