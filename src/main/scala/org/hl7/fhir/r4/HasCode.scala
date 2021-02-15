package org.hl7.fhir.r4



trait HasCode 


@annotation.implicitNotFound("Couldn't find implicit Code[${R},System]. Define it or ensure it is in scope")
sealed trait Code[T <: HasCode, S]{
  val value: BasicCodeableConcept[S] 
}
object Code
{
  def apply[T <: HasCode, S: Coding.System](implicit c: Code[T,S]) = c

  def apply[T <: HasCode](loinc: LOINC): Code[T,LOINC] =
    new Code[T,LOINC]{ val value = BasicCodeableConcept(loinc) }

  def apply[T <: HasCode, S: Coding.System](c: String, d: Option[String] = None): Code[T,S] =
    new Code[T,S]{ val value = BasicCodeableConcept(BasicCoding[S](c,d)) }

}


/*
@annotation.implicitNotFound("Couldn't find implicit Code[${R},System]. Define it or ensure it is in scope")
sealed trait Code[T <: HasCode]{
  type Sys
  val value: BasicCodeableConcept[Sys] 
}
object Code
{
  def apply[T <: HasCode](implicit c: Code[T]) = c

  def apply[T <: HasCode](loinc: LOINC): Code[T] =
    new Code[T]{
      type Sys = LOINC
      val value = BasicCodeableConcept(loinc)
    }

  def apply[T <: HasCode, S: Coding.System](c: String, d: Option[String] = None): Code[T] =
    new Code[T]{
      type Sys = S
      val value = BasicCodeableConcept(BasicCoding[S](c,d))
    }

}
*/
