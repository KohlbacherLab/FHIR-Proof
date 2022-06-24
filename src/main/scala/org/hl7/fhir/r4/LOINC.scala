package org.hl7.fhir.r4



sealed trait LOINC

object LOINC
{

  implicit val system =
    CodingSystem[LOINC]("http://loinc.org")


  def apply(
    code: String,
    display: Option[String] = None
  ): CodingStatic[LOINC] =
    CodingStatic(code,display,None)

  def apply(
    code: String,
    display: String
  ): CodingStatic[LOINC] =
    CodingStatic(code,Some(display),None)

}
