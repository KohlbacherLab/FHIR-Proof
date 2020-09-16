package org.hl7.fhir.r4




trait Organization extends Resource


object Organization
{

  implicit def organizationResourceType[Org <: Organization] =
    Resource.Type[Org]("Organization") 

}

