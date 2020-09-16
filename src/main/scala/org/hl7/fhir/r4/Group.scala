package org.hl7.fhir.r4





trait Group extends Resource

object Group
{

  implicit def groupType[G <: Group] =
    Resource.Type[G]("Group") 

}

