package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json



abstract class FamilyMemberHistory
extends DomainResource
with HasStatus[FamilyMemberHistory.Status.Value]
{
  val patient: Reference[Patient]

  val relationship: CodeableConcept with CodeableConcept.codingNel[CodingStatic[HL7v3FamilyMember.Value]]
}


final object FamilyMemberHistory
extends DomainResourceAttributes
{


  implicit def familyMemberHistoryResourceType[F <: FamilyMemberHistory] =
    Resource.Type[F]("FamilyMemberHistory") 


  object Status extends Enumeration
  {
    type Status = Value

    val Partial        = Value("partial")
    val Completed      = Value("completed")
    val EnteredInError = Value("entered-in-error")
    val HealthUnknown  = Value("health-unknown")

    implicit val format = Json.formatEnum(this)
  }

  //TODO TODO: other attributes....


}
