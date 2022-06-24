package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:, CNil}


abstract class Patient extends DomainResource with Individual


object Patient
extends DomainResourceAttributes
with IndividualAttributes
{

  implicit def patientResourceType[P <: Patient] =
    Resource.Type[P]("Patient") 


  sealed trait deceased[x]

  trait deceasedDateTime[D <: Temporal,C[_]] extends deceased[C[D]]{
    this: Patient =>
    val deceasedDateTime: C[D]
  }
  trait deceasedBoolean[C[_]] extends deceased[C[Boolean]]{
    this: Patient =>
    val deceasedBoolean: C[Boolean]
  }


  trait managingOrganization[C[_]]{
    this: Patient =>
    val managingOrganization: C[Reference[Organization]]
  }



  abstract class ContactElement extends BackboneElement[Many] 

  object Contact extends BackboneElementAttributes 
  {

    object RelationshipType extends Enumeration
    {
      type RelationshipType = Value
    
      val C = Value("C")
      val E = Value("E")
      val I = Value("I")
      val N = Value("N")
      val S = Value("S")
      val U = Value("U")

      implicit val system =
        CodingSystem[Value]("http://hl7.org/fhir/ValueSet/patient-contactrelationship")

      implicit val formatRelationshipType =
        Json.formatEnum(this)
    }
    
    trait relationship[C[_]] {
      this: ContactElement =>
      val relationship: C[List[CodeableConcept with CodeableConcept.codingNel[CodingStatic[RelationshipType.Value]]]] 
    }
    trait relationshipNel {
      this: ContactElement =>
      val relationship: NonEmptyList[CodeableConcept with CodeableConcept.codingNel[CodingStatic[RelationshipType.Value]]] 
    }

    trait organization[C[_]] {
      this: ContactElement =>
      val organization: C[Reference[Organization]]
    }

  }


  trait contact[+T <: ContactElement, C[+_]] {
    this: Patient =>
    val contact: C[List[T]]
  }
  trait contactNel[+T <: ContactElement] {
    this: Patient =>
    val contact: NonEmptyList[T]
  }



}
