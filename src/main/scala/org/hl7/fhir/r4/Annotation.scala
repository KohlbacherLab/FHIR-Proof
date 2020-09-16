package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList


abstract class Annotation extends Element
{
  val text: String
}

object Annotation extends ElementAttributes
{

  import shapeless.{:+:, CNil}

  type ValidAuthor[T] = T IsIn (Patient :+: Organization :+: CNil)


  sealed trait author[+X]

  trait authorReference[+R <: Resource, C[+_]] extends author[Reference[R]]{
    this: Annotation =>
    val authorReference: C[Reference[R]]
  }

  trait authorString[C[_]] extends author[String]{
    this: Annotation =>
    val authorString: C[String]
  }


  trait time[T <: Temporal, C[_]]{
    this: Annotation =>
    val time: C[T]
  }

}



trait CanHaveNotes
{

  trait note[+A <: Annotation, C[+_]]{
    this: DomainResource =>
    val note: C[List[A]]
  }

  trait noteNel[+A <: Annotation]{
    this: DomainResource =>
    val note: NonEmptyList[A]
  }

}
