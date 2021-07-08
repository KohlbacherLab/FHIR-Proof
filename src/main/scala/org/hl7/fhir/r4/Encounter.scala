package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:,CNil}



abstract class Encounter
extends DomainResource
with HasStatus[Encounter.Status.Value]
{
  val `class`: CodingStatic[HL7v3ActEncounterCode.Value]
//  val `class`: Coding[HL7v3ActEncounterCode.Value]
}


final object Encounter
extends DomainResourceAttributes
{


  implicit def encounterResourceType[E <: Encounter] =
    Resource.Type[E]("Encounter") 


  object Status extends CodedEnum
  {
    type Status = Value

    val Planned        = Val("planned","Planned")
    val Arrived        = Val("arrived","Arrived")
    val Triaged        = Val("triaged","Triaged")
    val InProgress     = Val("in-progress","In Progress")
    val OnLeave        = Val("onleave","On Leave")
    val Finished       = Val("finished","Finished")
    val Cancelled      = Val("cancelled","Cancelled")
    val EnteredInError = Val("entered-in-error","Entered in Error")
    val Unknown        = Val("unknown","Unknown")

    implicit val format = json.formatCodedEnum(this)
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
