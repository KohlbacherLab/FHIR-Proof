package org.hl7.fhir.r4


import play.api.libs.json.Json

object HL7v3FamilyMember extends Enumeration
{
  type HL7v3FamilyMember = Value

  val FAMMEMB = Value("FAMMEMB")
  val EXT     = Value("EXT")

  implicit val system =
    CodingSystem[Value]("http://terminology.hl7.org/ValueSet/v3-FamilyMember")

  implicit val format =
    Json.formatEnum(this)

}
