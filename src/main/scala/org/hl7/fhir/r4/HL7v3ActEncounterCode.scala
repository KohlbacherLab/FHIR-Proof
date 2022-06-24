package org.hl7.fhir.r4


import play.api.libs.json.Json

object HL7v3ActEncounterCode extends Enumeration
{
  type HL7v3ActEncounterCode = Value

  val AMB    = Value("AMB")
  val EMER   = Value("EMER")
  val FLD    = Value("FLD")
  val HH     = Value("HH")
  val IMP    = Value("IMP")
  val ACUTE  = Value("ACUTE")
  val NONAC  = Value("NONAC")
  val OBSENC = Value("OBSENC")
  val PRENC  = Value("PRENC")
  val SS     = Value("SS")
  val VR     = Value("VR")

  implicit val system = CodingSystem[Value]("http://terminology.hl7.org/ValueSet/v3-ActEncounterCode")

  implicit val format = Json.formatEnum(this)

}
