package org.hl7.fhir.r4


import java.time.temporal.Temporal


trait CanHaveOnset 
{

  this: DomainResourceAttributes =>


  sealed trait onset[X]

  trait onsetDateTime[D <: Temporal, C[_]] extends onset[C[D]]{
    this: DomainResource =>
    val onsetDateTime: C[D]
  }

  trait onsetPeriod[P <: Period[_], C[_]] extends onset[C[P]]{
    this: DomainResource =>
    val onsetPeriod: C[P]
  }

  trait onsetString[C[_]] extends onset[C[String]]{
    this: DomainResource =>
    val onsetString: C[String]
  }

}
