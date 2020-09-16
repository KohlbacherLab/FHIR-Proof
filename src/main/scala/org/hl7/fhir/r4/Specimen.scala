package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}



sealed trait HL7v2Table0487
object HL7v2Table0487
{
  implicit val system =
    Coding.System[HL7v2Table0487]("http://terminology.hl7.org/CodeSystem/v2-0487")
}



abstract class Specimen extends DomainResource


final object Specimen
extends DomainResourceAttributes
with CanHaveNotes
{

  implicit def specimenResourceType[S <: Specimen] =
    Resource.Type[S]("Specimen")


  trait accessionIdentifier[C[_]]{
    this: Specimen =>
    val accessionIdentifier: C[Identifier]
  }


  object Status extends CodedEnum
  {
    type Status = Value

    val Available      = Val("available","Available")
    val Unavailable    = Val("unavailable","Unavailable")
    val Unsatisfactory = Val("unsatisfactory","Unsatisfactory")
    val EnteredInError = Val("entered-in-error","Entered in Error")

    implicit val format = json.formatCodedEnum(this)
  }
  trait status[C[_]]{
    this: Specimen =>
    val status: C[Status.Value]
  }

  trait `type`[C[+_]]{
    this: Specimen =>
    val `type`: C[BasicCodeableConcept[HL7v2Table0487]]
  }


  type Subject[T] = T IsIn (Patient :+: Group :+: Device :+: Substance :+: CNil)

  trait subject[S <: Resource,C[_]]{
    this: Specimen =>
    val subject: C[Reference[S]]
  }


  trait receivedDateTime[D <: Temporal, C[_]]{
    this: Specimen =>
    val receivedDateTime: C[D]
  }


  abstract class CollectionElement extends BackboneElement[One]

  object Collection extends BackboneElementAttributes
  {

    trait collector[P <: Practitioner, C[_]]{
      this: CollectionElement =>
      val collector: C[Ref[P]]
    }

    sealed trait collected[x]
 
    trait collectedDateTime[D <: Temporal, C[_]] extends collected[C[D]]{
      this: CollectionElement =>
      val collectedDateTime: C[D]
    }

    trait collectedPeriod[P <: Period[_], C[_]] extends collected[C[P]]{
      this: CollectionElement =>
      val collectedPeriod: C[P]
    }

    trait method[+M <: CodeableConcept,C[+_]]{
      this: CollectionElement =>
      val method: C[M]
    }

    trait bodySite[+CC <: CodeableConcept, F[+_]]{
      this: CollectionElement =>
      val bodySite: F[CC]
    }

  }

  trait collection[C <: CollectionElement, F[_]]{
    this: Specimen =>
    val collection: F[C]
  }


  trait condition[+CC <: CodeableConcept, F[+_]]{
    this: Specimen =>
    val condition: F[List[CC]]
  }

  trait conditionNel[+CC <: CodeableConcept]{
    this: Specimen =>
    val condition: NonEmptyList[CC]
  }




}
