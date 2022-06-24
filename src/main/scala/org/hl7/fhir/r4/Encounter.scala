package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:,CNil}



abstract class Encounter
extends DomainResource
with HasStatus[Encounter.Status.Value]
{
  val `class`: CodingStatic[HL7v3ActEncounterCode.Value]
}


final object Encounter
extends DomainResourceAttributes
{


  implicit def encounterResourceType[E <: Encounter] =
    Resource.Type[E]("Encounter") 


  object Status extends Enumeration
  {
    type Status = Value

    val Planned        = Value("planned")
    val Arrived        = Value("arrived")
    val Triaged        = Value("triaged")
    val InProgress     = Value("in-progress")
    val OnLeave        = Value("onleave")
    val Finished       = Value("finished")
    val Cancelled      = Value("cancelled")
    val EnteredInError = Value("entered-in-error")
    val Unknown        = Value("unknown")

    implicit val format = Json.formatEnum(this)
  }


  abstract class StatusHistoryElement extends BackboneElement[Many]
  {
    val status: Status.Value
    val period: Period[_]
  }
  object StatusHistory extends BackboneElementAttributes


  trait statusHistory[+S <: StatusHistoryElement, C[+_]]{
    this: Encounter =>
    val statusHistory: C[List[S]]
  }
  trait statusHistoryNel[+S <: StatusHistoryElement]{
    this: Encounter =>
    val statusHistory: NonEmptyList[S]
  }


  trait period[C[+_]]{
    this: Encounter =>
    val period: C[Period[_]]
  }


  trait subject[+R <: Resource, C[+_]]{
    this: Encounter =>
    val subject: C[Reference[R]]
  }

  trait episodeOfCare[EoC <: EpisodeOfCare, C[+_]]{
    this: Encounter =>
    val episodeOfCare: C[List[Reference[EoC]]]
  }
  trait episodeOfCareNel[EoC <: EpisodeOfCare]{
    this: Encounter =>
    val episodeOfCare: NonEmptyList[Reference[EoC]]
  }



  abstract class DiagnosisElement[+C <: Resource: Diagnosis.ValidCondition] extends BackboneElement[Many]
  {
    val condition: Reference[C]
  }


  object Diagnosis extends BackboneElementAttributes
  {
    type ValidCondition[C] = C IsIn (Condition :+: CNil)
       //TODO: Procedure

  }

  trait diagnosis[+D <: DiagnosisElement[_], C[+_]]{
    this: Encounter =>
    val diagnosis: C[List[D]]
  }
  trait diagnosisNel[+D <: DiagnosisElement[_]]{
    this: Encounter =>
    val diagnosis: NonEmptyList[D]
  }


  trait serviceProvider[+Org <: Organization, C[+_]]{
    val serviceProvider: C[Reference[Org]]
  }

  trait partOf[+E <: Encounter, C[+_]]{
    this: Encounter =>
    val partOf: C[Reference[E]]
  }



}
