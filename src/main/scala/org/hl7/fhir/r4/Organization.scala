package org.hl7.fhir.r4




trait Organization extends DomainResource


object Organization
{

  implicit def organizationResourceType[Org <: Organization] =
    Resource.Type[Org]("Organization") 

}

