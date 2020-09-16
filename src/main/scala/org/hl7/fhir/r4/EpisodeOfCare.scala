package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:,CNil}



abstract class EpisodeOfCare
extends DomainResource
with HasStatus[EpisodeOfCare.Status.Value]
{
  val patient: Reference[Patient]
}

final object EpisodeOfCare
extends DomainResourceAttributes
{


  implicit def episodeOfCareResourceType[E <: EpisodeOfCare] =
    Resource.Type[E]("EpisodeOfCare") 


  object Status extends CodedEnum
  {
    type Status = Value

    val Planned        = Val("planned","Planned")
    val Waitlist       = Val("waitlist","Waitlist")
    val Active         = Val("active","Active")
    val OnHold         = Val("onhold","On Hold")
    val Finished       = Val("finished","Finished")
    val Cancelled      = Val("cancelled","Cancelled")
    val EnteredInError = Val("entered-in-error","Entered in Error")

    implicit val format = json.formatCodedEnum(this)
  }


  abstract class StatusHistoryElement extends BackboneElement[Many]
  {
    val status: Status.Value
    val period: Period[_]
  }
  object StatusHistory extends BackboneElementAttributes


  trait statusHistory[+S <: StatusHistoryElement, C[+_]]{
    this: EpisodeOfCare =>
    val statusHistory: C[List[S]]
  }
  trait statusHistoryNel[+S <: StatusHistoryElement]{
    this: EpisodeOfCare =>
    val statusHistory: NonEmptyList[S]
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
    this: EpisodeOfCare =>
    val diagnosis: C[List[D]]
  }

  trait diagnosisNel[+D <: DiagnosisElement[_]]{
    this: EpisodeOfCare =>
    val diagnosis: NonEmptyList[D]
  }


  trait managingOrganization[+Org <: Organization, C[+_]]{
    val managingOrganization: C[Reference[Org]]
  }

  trait period[C[+_]]{
    this: EpisodeOfCare =>
    val period: C[Period[_]]
  }



}
