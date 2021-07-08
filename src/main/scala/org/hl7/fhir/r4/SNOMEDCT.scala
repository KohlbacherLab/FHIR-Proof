package org.hl7.fhir.r4




final case class SNOMEDCT(
  code: String,
  display: Option[String]
) extends Coded

object SNOMEDCT
{
  implicit val system = CodingSystem[SNOMEDCT]("http://snomed.info/sct")
//  implicit val system = Coding.System[SNOMEDCT]("http://snomed.info/sct")
}
  


