package org.hl7.fhir.r4



trait HasCode[S] 
{
  val code: CodeableConceptStatic[S]
}



trait HasStaticCode 

@annotation.implicitNotFound("Couldn't find implicit Code[${R},System]. Define it or ensure it is in scope")
sealed trait Code[T <: HasStaticCode, S]{
  val value: CodeableConceptStatic[S] 
}

object Code
{
  def apply[T <: HasStaticCode, S: CodingSystem](implicit c: Code[T,S]) = c

  def apply[T <: HasStaticCode](
    loinc: CodingStatic[LOINC]
  ): Code[T,LOINC] =
    new Code[T,LOINC]{
      val value = CodeableConceptStatic(loinc)
    }
/*
  def apply[T <: HasStaticCode](
    loinc: LOINC
  ): Code[T,LOINC] =
    new Code[T,LOINC]{
      val value = CodeableConceptStatic(loinc)
    }
*/

  def apply[T <: HasStaticCode, S: CodingSystem](
    c: String,
    d: Option[String] = None
  ): Code[T,S] =
    new Code[T,S]{
      val value = CodeableConceptStatic(CodingStatic[S](c,d,None))
    }

  def apply[T <: HasStaticCode, S: CodingSystem](
    c: String, d: String
  ): Code[T,S] =
    Code[T,S](c,Some(d))

}

