package org.hl7.fhir.r4



object AdministrativeGender extends CodedEnum
{
  type AdministrativeGender = Value

  val Male    = Val("male","Male")
  val Female  = Val("female","Female")
  val Other   = Val("other","Other")
  val Unknown = Val("unknown","Unknown")

  implicit val formatGender =
    json.formatCodedEnum(this)

}

