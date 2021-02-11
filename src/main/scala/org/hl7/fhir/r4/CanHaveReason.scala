package org.hl7.fhir.r4


import cats.data.NonEmptyList


//trait CanHaveReason
trait CanHaveReason[R <: DomainResource]
{

  this: DomainResourceAttributes =>


  trait reasonReference[C[+_]]{
    this: R =>
//    this: DomainResource =>
    val reasonReference: C[List[Reference[_]]]
  }

  trait reasonReferenceNel{
    this: R =>
//    this: DomainResource =>
    val reasonReference: NonEmptyList[Reference[_]]
  }

}
