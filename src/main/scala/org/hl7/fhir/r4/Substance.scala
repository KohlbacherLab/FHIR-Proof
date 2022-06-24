package org.hl7.fhir.r4


import play.api.libs.json.Json


abstract class Substance
extends DomainResource
with Substance.code


final object Substance
extends DomainResourceAttributes
{

  implicit def substanceResourceType[S <: Substance] =
    Resource.Type[S]("Substance")


  object Status extends Enumeration {

    type Status = Value    

    val Active         = Value("active")
    val Inactive       = Value("inactive")
    val EnteredInError = Value("entered-in-error")

    implicit val format = Json.formatEnum(this)
  }

  trait status[C[_]] extends Substance {
    
    val status: C[Status.Value]
  }

  trait code {
    val code: CodeableConcept with CodeableConcept.codingNel[CodingStatic[SNOMEDCT]]
  }


}
