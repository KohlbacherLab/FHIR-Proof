package org.hl7.fhir.r4




final case class LOINC(
  code: String,
  display: Option[String] = None
) extends Coded


object LOINC
{

  implicit val system = Coding.System[LOINC]("http://loinc.org")


  sealed trait Code[T <: HasLOINCCode]{ val value: BasicCodeableConcept[LOINC] }
  object Code
  {
    def apply[T <: HasLOINCCode](implicit c: Code[T]) = c

    def apply[T <: HasLOINCCode](loinc: LOINC) =
      new Code[T]{ val value = BasicCodeableConcept(loinc) }

    def apply[T <: HasLOINCCode](c: String, d: Option[String] = None) =
      new Code[T]{ val value = BasicCodeableConcept(LOINC(c,d)) }
  }

}


trait HasLOINCCode
//trait HasLOINCCode {
//  val code: BasicCodeableConcept[LOINC]
//}



