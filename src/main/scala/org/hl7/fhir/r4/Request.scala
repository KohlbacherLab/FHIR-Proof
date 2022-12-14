package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

import shapeless.{:+:, CNil}



trait Request
{
  this: DomainResource =>
}


trait RequestAttributes
extends CanHaveNotes
{
  self =>


  object Priority extends Enumeration
  {
     type Priority = Value

     val Routine = Value("routine")
     val Urgent  = Value("urgent")
     val Asap    = Value("asap")
     val Stat    = Value("stat")

     implicit val format = Json.formatEnum(this)
  }

  trait priority[C[_]]{
    this: Request =>
    val priority: C[Priority.Value]
  }

  trait doNotPerform[C[_]]{
    this: Request =>
    val doNotPerform: C[Boolean]
  }

/*
  trait basedOn[R <: Resource with Request,C[_]]{
    this: Request =>
    val basedOn: C[List[Reference[R]]]
  }
  trait basedOnNel[R <: Resource with Request]{
    this: Request =>
    val basedOn: NonEmptyList[Reference[R]]
  }
*/


  type StatusType

  trait status {
    this: Request =>
    val status: self.StatusType
  }


  type IntentType

  trait intent {
    this: Request =>
    val intent: self.IntentType
  }


  type Subjects   = Patient :+: Group :+: CNil
  type Subject[T] = T IsIn Subjects

  trait subject[S <: Resource]{
    this: Request =>
    val subject: Reference[S]
  }


  trait authoredOn[D <: Temporal,C[_]]{
    this: Request =>
    val authoredOn: C[D]
  }

}

