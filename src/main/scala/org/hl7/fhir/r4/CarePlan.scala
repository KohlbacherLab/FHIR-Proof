package org.hl7.fhir.r4



import java.time.temporal.Temporal

import cats.data.NonEmptyList
import play.api.libs.json.Json

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


  object Status extends Enumeration
  {
     type Status = Value

     val Draft          = Value("draft")
     val Active         = Value("active")
     val OnHold         = Value("on-hold")
     val Revoked        = Value("revoked")
     val Completed      = Value("completed")
     val EnteredInError = Value("entered-in-error")
     val Unknown        = Value("unknown")

     implicit val format = Json.formatEnum(this)
  }
  type StatusType = Status.Value


  object Intent extends Enumeration
  {
     type Intent = Value

     val Proposal      = Value("proposal")
     val Plan          = Value("plan")
     val Order         = Value("order")
     val Option        = Value("option")

     implicit val format = Json.formatEnum(this)
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
    {
      val status: Detail.Status.Value
    }

    object Detail extends BackboneElementAttributes
    with CanHaveReason[DetailElement]
    {
      //TODO: attributes

      trait code[+CC <: CodeableConcept,C[+_]]{
        this: DetailElement =>
        val code: C[CC]
      }

      trait reasonCode[+CC <: CodeableConcept,C[+_]]{
        this: DetailElement =>
        val reasonCode: C[List[CC]]
      }
      trait reasonCodeNel[+CC <: CodeableConcept]{
        this: DetailElement =>
        val reasonCode: NonEmptyList[CC]
      }


      object Status extends Enumeration
      {
         type Status = Value
    
         val NotStarted     = Value("not-started")
         val Scheduled      = Value("scheduled")
         val InProgress     = Value("in-progress")
         val OnHold         = Value("on-hold")
         val Completed      = Value("completed")
         val Cancelled      = Value("cancelled")
         val Stopped        = Value("stopped")
         val EnteredInError = Value("entered-in-error")
         val Unknown        = Value("unknown")
    
         implicit val format = Json.formatEnum(this)
      }

      trait statusReason[+CC <: CodeableConcept,C[+_]]{
        this: DetailElement =>
        val statusReason: C[CC]
      }

      trait description[C[_]]{
        this: DetailElement =>
        val description: C[String]
      }


    }

  }

  trait ActivitySet {
    this: Product =>
  }

  trait activity[+A <: ActivityElement,C[+_]]{
    this: CarePlan =>
    val activity: C[List[A]]
  }

  trait activities[+A <: ActivitySet,C[+_]]{
    this: CarePlan =>
    val activity: C[A]
  }

  trait activityNel[+A <: ActivityElement]{
    this: CarePlan =>
    val activity: NonEmptyList[A]
  }







}
