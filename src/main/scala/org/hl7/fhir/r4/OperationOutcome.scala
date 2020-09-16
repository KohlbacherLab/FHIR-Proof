package org.hl7.fhir.r4



import cats.data.NonEmptyList

import play.api.libs.json.Json



abstract class OperationOutcome extends DomainResource

object OperationOutcome
extends DomainResourceAttributes
{

  implicit def resourceTypeOperationOutcome[O <: OperationOutcome] =
    Resource.Type[O]("OperationOutcome")


  abstract class IssueElement extends BackboneElement[Many]

  object Issue
  {

    object Severity extends Enumeration
    {
      val Fatal       = Value("fatal")
      val Error       = Value("error")
      val Warning     = Value("warning")
      val Information = Value("information")

      implicit val format = Json.formatEnum(this)    
    }

    trait severity[C[_]]{
      this: IssueElement =>
      val severity: C[Severity.Value]
    }
  
  }

  trait issue[+I <: IssueElement]{
    this: OperationOutcome =>
    val issue: NonEmptyList[I]
  }


}
