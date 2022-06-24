package org.hl7.fhir.r4


import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

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


  object Status extends Enumeration
  {
     type Status = Value

     val Active         = Value("active")
     val Completed      = Value("completed")
     val EnteredInError = Value("entered-in-error")
     val Intended       = Value("intended")
     val Stopped        = Value("stopped")
     val OnHold         = Value("on-hold")
     val Unknown        = Value("unknown")
     val NotTaken       = Value("not-taken")

     implicit val format = Json.formatEnum(this)
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
