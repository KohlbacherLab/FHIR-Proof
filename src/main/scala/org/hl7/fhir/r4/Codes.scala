package org.hl7.fhir.r4




object ProcessPriority extends CodedEnum
{
   type ProcessPriority = Value

   val Stat     = Val("stat"    ,"Immediate")
   val Normal   = Val("normal"  ,"Normal")
   val Deferred = Val("deferred","Deferred")

   implicit val system =
     Coding.System[ProcessPriority]("http://terminology.hl7.org/CodeSystem/processpriority")

   implicit val format = json.formatCodedEnum(this)
}

