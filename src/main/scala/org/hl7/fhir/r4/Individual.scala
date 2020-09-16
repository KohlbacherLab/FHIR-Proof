package org.hl7.fhir.r4



import java.time.LocalDate


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

  trait birthDate[C[_]]{
    this: Individual =>
    val birthDate: C[LocalDate]
  }

}
