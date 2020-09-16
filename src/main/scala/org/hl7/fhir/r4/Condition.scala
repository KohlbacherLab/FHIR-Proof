package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}



abstract class Condition
extends DomainResource
with Event
{
  this: Condition.subject[_,Required] =>
}


final object Condition
extends DomainResourceAttributes
with EventAttributes[Patient :+: Group :+: CNil]
with CanHaveEncounter
with CanHaveOnset
{

  implicit def conditionResourceType[C <: Condition] =
    Resource.Type[C]("Condition") 


  trait recordedDate[D <: Temporal,C[_]]{
    this: Condition =>
    val recordedDate: C[D]
  }

  trait code[+CC <: CodeableConcept,C[+_]]{
    this: Condition =>
    val code: C[CC]
  }

  trait bodySite[+CC <: CodeableConcept, C[+_]]{
    this: Condition =>
    val bodySite: C[List[CC]]
  }
  trait bodySiteNel[+CC <: CodeableConcept]{
    this: Condition =>
    val bodySite: NonEmptyList[CC]
  }


  abstract class StageElement extends BackboneElement[Many]
  {
    this: Stage.value[_] =>
  }

  object Stage extends BackboneElementAttributes
  {

    type ValidValue[V] = V IsIn (CodeableConcept :+: Ref[Observation] :+: CNil)  //TODO: Diag.Report, ClinicalImpression

    sealed trait value[+X]

    trait summary[+CC <: CodeableConcept] extends value[CC]{
      val summary: CC
    }

    trait assessment[+R <: Resource] extends value[Reference[R]] {
      val assessment: NonEmptyList[Reference[R]]
    }
  }

  trait stage[+S <: StageElement, C[+_]]{
    this: Condition =>
    val stage: C[List[S]]
  }
  trait stages[+S <: Product, C[+_]]{
    this: Condition =>
    val stage: C[S]
  }
  trait stageNel[+S <: StageElement]{
    this: Condition =>
    val stage: NonEmptyList[S]
  }



  abstract class EvidenceElement extends BackboneElement[Many]
  {
    this: Evidence.value[_] =>
  }

  object Evidence extends BackboneElementAttributes
  {

    type ValidValue[V] = V IsIn (CodeableConcept :+: Ref[_] :+: CNil)

    sealed trait value[+X]

    trait code[+T <: CodeableConcept] extends value[T]{
      val code: NonEmptyList[T]
    }

    trait detail[+R <: Resource] extends value[Reference[R]] {
      val detail: NonEmptyList[Reference[R]]
    }
  }

  trait evidence[+E <: EvidenceElement, C[+_]]{
    this: Condition =>
    val evidence: C[List[E]]
  }
  trait evidenceNel[+E <: EvidenceElement]{
    this: Condition =>
    val evidence: NonEmptyList[E]
  }

}
