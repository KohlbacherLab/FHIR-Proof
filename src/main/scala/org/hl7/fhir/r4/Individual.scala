package org.hl7.fhir.r4



//import java.time.LocalDate
import java.time.temporal.Temporal


trait Individual {
  this: DomainResource =>
}


trait IndividualAttributes
{

  this: DomainResourceAttributes =>


  trait active[C[_]]{
    this: Individual =>
    val active: C[Boolean]
  }

  trait gender[C[_]]{
    this: Individual =>
    val gender: C[AdministrativeGender.Value]
  }

  trait birthDate[D <: Temporal, C[+_]]{
    this: Individual =>
    val birthDate: C[D]
  }

}
