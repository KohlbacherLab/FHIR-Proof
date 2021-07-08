package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList

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

    object RelationshipType extends CodedEnum
    {
      type RelationshipType = Value
    
      val C = Val("C","Emergency Contact")
      val E = Val("E","Employer")
      val I = Val("I","Insurance Company")
      val N = Val("N","Next-of-kin")
      val S = Val("S","State Agency")
      val U = Val("U","Unknown")

      implicit val system =
        CodingSystem[Value]("http://hl7.org/fhir/ValueSet/patient-contactrelationship")
//        Coding.System[Value]("http://hl7.org/fhir/ValueSet/patient-contactrelationship")

      implicit val formatRelationshipType =
        json.formatCodedEnum(this)
    }
    
    trait relationship[C[_]] {
      this: ContactElement =>
      val relationship: C[List[CodeableConcept with CodeableConcept.codingNel[CodingStatic[RelationshipType.Value]]]] 
//      val relationship: C[List[CodeableConcept with CodeableConcept.codingNel[Coding[RelationshipType.Value]]]] 
    }
    trait relationshipNel {
      this: ContactElement =>
      val relationship: NonEmptyList[CodeableConcept with CodeableConcept.codingNel[CodingStatic[RelationshipType.Value]]] 
//      val relationship: NonEmptyList[CodeableConcept with CodeableConcept.codingNel[Coding[RelationshipType.Value]]] 
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
