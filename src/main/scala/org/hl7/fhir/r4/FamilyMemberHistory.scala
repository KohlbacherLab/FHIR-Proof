package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList



abstract class FamilyMemberHistory
extends DomainResource
with HasStatus[FamilyMemberHistory.Status.Value]
{
  val patient: Reference[Patient]

  val relationship: CodeableConcept with CodeableConcept.codingNel[Coding[HL7v3FamilyMember.Value]]
}


final object FamilyMemberHistory
extends DomainResourceAttributes
{


  implicit def familyMemberHistoryResourceType[F <: FamilyMemberHistory] =
    Resource.Type[F]("FamilyMemberHistory") 


  object Status extends CodedEnum
  {
    type Status = Value

    val Partial        = Val("partial","Partial")
    val Completed      = Val("completed","Completed")
    val EnteredInError = Val("entered-in-error","Entered in Error")
    val HealthUnknown  = Val("health-unknown","Health Unknown")

    implicit val format = json.formatCodedEnum(this)
  }

  //TODO TODO: other attributes....


}
