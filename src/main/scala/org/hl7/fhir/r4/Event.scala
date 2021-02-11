package org.hl7.fhir.r4



import shapeless.Coproduct


trait Event
{
  this: DomainResource =>
}


trait CanHaveBasedOn
{
  this: DomainResourceAttributes =>

  trait basedOn[+R <: Resource with Request, C[+_]]{
    this: Event =>
    val basedOn: C[List[Reference[R]]]
  }

  trait basedOnNel[+R <: Resource with Request]{
    this: Event =>
    val basedOn: cats.data.NonEmptyList[Reference[R]]
  }

}

trait CanHavePartOf
{
  this: DomainResourceAttributes =>

  trait partOf[+R <: Resource with Event, C[+_]]{
    this: Event =>
    val partOf: C[List[Reference[R]]]
  }

  trait partOfNel[+R <: Resource with Event]{
    this: Event =>
    val partOf: cats.data.NonEmptyList[Reference[R]]
  }

}

trait EventAttributes[Subjects <: Coproduct]
extends CanHaveNotes
{

//  trait partOf



  type Subject[T] = T IsIn Subjects

  trait subject[+T <: Resource,C[+_]]{
    this: Event =>
    val subject: C[Reference[T]]
  }

}

