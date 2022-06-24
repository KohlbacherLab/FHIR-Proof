package org.hl7.fhir.r4


import java.time.Instant
import java.time.temporal.Temporal

import shapeless.{:+:,CNil}

import cats.data.NonEmptyList
import play.api.libs.json.Json



sealed abstract class Observation
extends DomainResource
   with Event
   with HasStatus[Observation.Status.Value]

abstract class ObservationSC
extends Observation
   with HasStaticCode

abstract class ObservationDC[S]
extends Observation
   with HasCode[S]



final object Observation
extends DomainResourceAttributes
with EventAttributes[Patient :+: Group :+: Device :+: CNil]
with CanHavePartOf
with CanHaveEffective[Temporal :+: Period[_] :+: CNil]
with CanHaveEncounter
with CanHaveNotes
with CanHaveValue[
  CodeableConcept :+:
  Quantity :+:
  String :+:
  Boolean :+:
  Int :+:
  Period[_] :+:
  Range :+:
  Ratio :+:
  CNil
] //TODO: other valid value types
{


  implicit def observationResourceType[O <: Observation] =
    Resource.Type[O]("Observation") 


  object Status extends Enumeration
  {
    val Registered  = Value("registered")
    val Preliminary = Value("preliminary")
    val Final       = Value("final")
    val Amended     = Value("amended") 

    implicit val format = Json.formatEnum(this)
  }

  type StatusType = Status.Value



  trait category[+CC <: CodeableConcept, F[+_]]{
    this: Observation =>
    val category: F[List[CC]]
  }
  trait categoryNel[+CC <: CodeableConcept]{
    this: Observation =>
    val category: NonEmptyList[CC]
  }


  trait issued[C[_]]{
    this: Observation =>
    val issued: Instant
  }


  
  trait interpretation[+CC <: CodeableConcept, F[+_]]{
    this: Observation =>
    val interpretation: F[List[CC]]
  }   
  trait interpretationNel[+CC <: CodeableConcept]{
    this: Observation =>
    val interpretation: NonEmptyList[CC]
  }   



  trait bodySite[+CC <: CodeableConcept, F[+_]]{
    this: Observation =>
    val bodySite: F[CC]
  }


  trait method[+CC <: CodeableConcept, F[+_]]{
    this: Observation =>
    val method: F[CC]
  }


  trait specimen[C[+_]]{
    this: Observation =>
    val specimen: C[Reference[Specimen]]
  }


  sealed abstract class ComponentElement extends BackboneElement[Many]

  abstract class ComponentElementSC
  extends ComponentElement
     with HasStaticCode

  abstract class ComponentElementDC[S]
  extends ComponentElement
     with HasCode[S]


  object Component
  extends BackboneElementAttributes
  with CanHaveValue[
    CodeableConcept :+: String :+: Boolean :+:
    Quantity :+: Range :+: Ratio :+:
    Int :+: Period[_] :+: CNil
  ] //TODO: other valid value types
  {

    trait interpretation[+CC <: CodeableConcept, F[+_]]{
      this: ComponentElement =>
      val interpretation: F[List[CC]]
    }   
    trait interpretationNel[+CC <: CodeableConcept]{
      this: ComponentElement =>
      val interpretation: NonEmptyList[CC]
    }   

  }


  trait component[+C <: ComponentElement, F[+_]]{
    this: Observation =>
    val component: F[List[C]]
  }
  trait componentNel[+C <: ComponentElement]{
    this: Observation =>
    val component: NonEmptyList[C]
  }
  trait components[+C <: Product, F[+_]]{
    this: Observation =>
    val component: F[C]
  }


}
