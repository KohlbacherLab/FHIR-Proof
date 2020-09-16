package org.hl7.fhir.r4




trait CanHaveEncounter
{
  this: DomainResourceAttributes =>


  trait encounter[E <: Encounter, C[+_]]
  {
    this: DomainResource =>
    val encounter: C[Reference[E]]
  }

}
