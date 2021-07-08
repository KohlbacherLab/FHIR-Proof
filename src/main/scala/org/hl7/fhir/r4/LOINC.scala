package org.hl7.fhir.r4




final case class LOINC(
  code: String,
  display: Option[String] = None
) extends Coded


object LOINC
{

  implicit val system = CodingSystem[LOINC]("http://loinc.org")
//  implicit val system = Coding.System[LOINC]("http://loinc.org")

}


//trait HasLOINCCode


