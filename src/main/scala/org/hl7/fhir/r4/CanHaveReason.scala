package org.hl7.fhir.r4


import cats.data.NonEmptyList


trait CanHaveReason[R <: Extensible]
{

  this: ExtensibleAttributes =>


  trait reasonReference[C[+_]]{
    this: R =>
    val reasonReference: C[List[Reference[_]]]
  }

  trait reasonReferenceNel{
    this: R =>
    val reasonReference: NonEmptyList[Reference[_]]
  }

}


/*
trait CanHaveReason[R <: DomainResource]
{

  this: DomainResourceAttributes =>


  trait reasonReference[C[+_]]{
    this: R =>
    val reasonReference: C[List[Reference[_]]]
  }

  trait reasonReferenceNel{
    this: R =>
    val reasonReference: NonEmptyList[Reference[_]]
  }

}
*/
