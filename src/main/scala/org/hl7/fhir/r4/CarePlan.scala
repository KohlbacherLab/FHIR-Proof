package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList

import shapeless.{:+:, CNil}



abstract class CarePlan
extends DomainResource
   with Request
   with CarePlan.status
   with CarePlan.intent
{
  this: CarePlan.subject[_] =>
}


final object CarePlan
extends DomainResourceAttributes
with RequestAttributes
with CanHaveEncounter
{


  implicit def carePlanResourceType[C <: CarePlan] =
    Resource.Type[C]("CarePlan")


  object Status extends CodedEnum
  {
     type Status = Value

     val Draft          = Val("draft","Draft")
     val Active         = Val("active","Active")
     val OnHold         = Val("on-hold","On Hold")
     val Revoked        = Val("revoked","Revoked")
     val Completed      = Val("completed","Completed")
     val EnteredInError = Val("entered-in-error","Entered in Error")
     val Unknown        = Val("unknown","Unknown")

     implicit val format = json.formatCodedEnum(this)
  }
  type StatusType = Status.Value


  object Intent extends CodedEnum
  {
     type Intent = Value

     val Proposal      = Val("proposal","Proposal")
     val Plan          = Val("plan","Plan")
     val Order         = Val("order","Order")
     val Option        = Val("option","option")

     implicit val format = json.formatCodedEnum(this)
  }
  type IntentType = Intent.Value



  trait basedOn[+C <: CarePlan,F[+_]]{
    this: CarePlan =>
    val basedOn: F[List[Reference[C]]]
  }
  trait basedOnNel[+C <: CarePlan]{
    this: CarePlan =>
    val basedOn: NonEmptyList[Reference[C]]
  }


  trait created[D <: Temporal,C[_]]{
    this: CarePlan =>
    val created: C[D]
  }


  trait description[C[_]]{
    this: CarePlan =>
    val description: C[String]
  }

  trait addresses[F[+_]]{
    this: CarePlan =>
    val addresses: F[List[Reference[Condition]]]
  }
  trait addressesNel{
    this: CarePlan =>
    val addresses: NonEmptyList[Reference[Condition]]
  }

 
  abstract class ActivityElement extends BackboneElement[Many]
  {
    this: Activity.content[_] =>
  }
  object Activity extends BackboneElementAttributes
  {

    sealed trait content[+X]

    trait reference[+R <: Resource] extends content[Reference[R]]{
      this: ActivityElement =>
      val reference: Reference[R]
    }

    trait detail[+D <: DetailElement] extends content[D]{
      this: ActivityElement =>
      val detail: D
    }


    abstract class DetailElement extends BackboneElement[One]

    object Detail extends BackboneElementAttributes
    {
      //TODO: attributes
    }

  }

  trait activity[+A <: ActivityElement,C[+_]]{
    this: CarePlan =>
    val activity: C[List[A]]
  }

  trait activityNel[+A <: ActivityElement]{
    this: CarePlan =>
    val activity: NonEmptyList[A]
  }







}
