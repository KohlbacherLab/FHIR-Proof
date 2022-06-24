package org.hl7.fhir.r4


import java.time.Instant
import java.time.temporal.Temporal

import shapeless.{:+:,CNil}

import cats.data.NonEmptyList
import play.api.libs.json.Json



sealed abstract class ResearchStudy
extends DomainResource
   with HasStatus[ResearchStudy.Status.Value]


final object ResearchStudy
extends DomainResourceAttributes
with CanHavePartOf
with CanHaveNotes
{


  implicit def researchStudyResourceType[R <: ResearchStudy] =
    Resource.Type[R]("ResearchStudy") 


  object Status extends Enumeration
  {

    //TODO TODO

    implicit val format = Json.formatEnum(this)
  }

  //TODO TODO

}
