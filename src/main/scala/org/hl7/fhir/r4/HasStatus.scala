package org.hl7.fhir.r4



trait HasStatus[S]
{
  this: DomainResource =>

  val status: S
}
