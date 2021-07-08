package org.hl7.fhir.r4



object HL7v3ActEncounterCode extends CodedEnum
{
  type HL7v3ActEncounterCode = Value

  val AMB    = Val("AMB","ambulatory")
  val EMER   = Val("EMER","emergency")
  val FLD    = Val("FLD","field")
  val HH     = Val("HH","home health")
  val IMP    = Val("IMP","inpatient encounter")
  val ACUTE  = Val("ACUTE","inpatient acute")
  val NONAC  = Val("NONAC","inpatient non-acute")
  val OBSENC = Val("OBSENC","observation encounter")
  val PRENC  = Val("PRENC","pre-admission")
  val SS     = Val("SS","short stay")
  val VR     = Val("VR","virtual")

  implicit val system = CodingSystem[Value]("http://terminology.hl7.org/ValueSet/v3-ActEncounterCode")
//  implicit val system = Coding.System[Value]("http://terminology.hl7.org/ValueSet/v3-ActEncounterCode")

  implicit val format = json.formatCodedEnum(this)

}
