package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}



abstract class MedicationStatement
extends DomainResource
with Event
with HasStatus[MedicationStatement.Status.Value]
{
  this: MedicationStatement.subject[_,Required]
        with MedicationStatement.medication[_] =>
}


final object MedicationStatement
extends DomainResourceAttributes
with EventAttributes[Patient :+: Group :+: CNil]
with CanHaveBasedOn
with CanHaveReason[MedicationStatement]
with CanHaveEffective[Temporal :+: Period[_] :+: CNil]
with CanHaveNotes
with HasMedication
{

  implicit def medicationStatementResourceType[M <: MedicationStatement] =
    Resource.Type[M]("MedicationStatement")


  object Status extends CodedEnum
  {
     type Status = Value

     val Active         = Val("active","Active")
     val Completed      = Val("completed","Completed")
     val EnteredInError = Val("entered-in-error","Entered in Error")
     val Intended       = Val("intended","Intended")
     val Stopped        = Val("stopped","Stopped")
     val OnHold         = Val("on-hold","On Hold")
     val Unknown        = Val("unknown","Unknown")
     val NotTaken       = Val("not-taken","Not Taken")

     implicit val format = json.formatCodedEnum(this)
  }

  type StatusType = Status.Value


  trait statusReason[+T <: CodeableConcept,C[+_]] {
    this: MedicationStatement =>
    val statusReason: C[List[T]]
  }
  trait statusReasonNel[+T <: CodeableConcept] {
    this: MedicationStatement =>
    val statusReason: NonEmptyList[T]
  } 


  trait context[+R <: Resource, C[+_]]{
    this: MedicationStatement =>
    val context: C[Reference[R]]
  }


  trait dateAsserted[D <: Temporal, C[_]]{
    this: MedicationStatement =>
    val dateAsserted: C[D]
  }


  trait dosage[+D <: Dosage, C[+_]]{
    this: MedicationStatement =>
    val dosage: C[List[D]]
  }
  trait dosageNel[+D <: Dosage]{
    this: MedicationStatement =>
    val dosage: NonEmptyList[D]
  }


}
