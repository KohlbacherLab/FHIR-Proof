package org.hl7.fhir.r4


import java.time.Instant
import java.time.temporal.Temporal

import shapeless.{:+:,CNil}

import cats.data.NonEmptyList



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


  object Status extends CodedEnum
  {

    //TODO TODO

    implicit val format = json.formatCodedEnum(this)
  }

  //TODO TODO

}
