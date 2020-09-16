package org.hl7.fhir.r4



trait HasMedication
{

  sealed trait medication[+X]

  trait medicationCodeableConcept[+T <: CodeableConcept] extends medication[T]{
    this: DomainResource =>
    val medicationCodeableConcept: T
  }

  trait medicationReference[+M <: Medication] extends medication[Reference[M]]{
    this: DomainResource =>
    val medicationReference: Reference[M]
  }

}
