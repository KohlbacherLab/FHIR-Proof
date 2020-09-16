package org.hl7.fhir.r4



abstract class Substance
extends DomainResource
with Substance.code


final object Substance
extends DomainResourceAttributes
{

  implicit def substanceResourceType[S <: Substance] =
    Resource.Type[S]("Substance")


  object Status extends CodedEnum {

    type Status = Value    

    val Active         = Val("active","Active")
    val Inactive       = Val("inactive","Inactive")
    val EnteredInError = Val("entered-in-error","Entered in Error")

    implicit val format = json.formatCodedEnum(this)
  }

  trait status[C[_]] extends Substance {
    
    val status: C[Status.Value]
  }

  trait code {
    val code: CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]
  }


}
