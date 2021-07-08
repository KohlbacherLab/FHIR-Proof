package org.hl7.fhir.r4



object HL7v3FamilyMember extends CodedEnum
{
  type HL7v3FamilyMember = Value

  val FAMMEMB = Val("FAMMEMB","family member")
  val EXT     = Val("EXT"    ,"extended family member")

  implicit val system =
//    Coding.System[Value]("http://terminology.hl7.org/ValueSet/v3-FamilyMember")
    CodingSystem[Value]("http://terminology.hl7.org/ValueSet/v3-FamilyMember")

  implicit val format =
    json.formatCodedEnum(this)

}
