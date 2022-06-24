package org.hl7.fhir.r4


import play.api.libs.json.Json


object AdministrativeGender extends Enumeration
{
  type AdministrativeGender = Value

  val Male    = Value("male")
  val Female  = Value("female")
  val Other   = Value("other")
  val Unknown = Value("unknown")

  implicit val format =
    Json.formatEnum(this)

}

