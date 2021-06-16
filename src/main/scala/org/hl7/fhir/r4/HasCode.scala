package org.hl7.fhir.r4



trait HasCode[S] 
{
  val code: BasicCodeableConcept[S]
}



trait HasStaticCode 

@annotation.implicitNotFound("Couldn't find implicit Code[${R},System]. Define it or ensure it is in scope")
sealed trait Code[T <: HasStaticCode, S]{
  val value: BasicCodeableConcept[S] 
}
object Code
{
  def apply[T <: HasStaticCode, S: Coding.System](implicit c: Code[T,S]) = c

  def apply[T <: HasStaticCode](loinc: LOINC): Code[T,LOINC] =
    new Code[T,LOINC]{ val value = BasicCodeableConcept(loinc) }

  def apply[T <: HasStaticCode, S: Coding.System](c: String, d: Option[String] = None): Code[T,S] =
    new Code[T,S]{ val value = BasicCodeableConcept(BasicCoding[S](c,d)) }

}

