package org.hl7.fhir.r4


import play.api.libs.json.Json

object ProcessPriority extends Enumeration
{
   type ProcessPriority = Value

   val Stat     = Value("stat")
   val Normal   = Value("normal")
   val Deferred = Value("deferred")

   implicit val system =
     CodingSystem[ProcessPriority]("http://terminology.hl7.org/CodeSystem/processpriority")

   implicit val format = Json.formatEnum(this)
}

