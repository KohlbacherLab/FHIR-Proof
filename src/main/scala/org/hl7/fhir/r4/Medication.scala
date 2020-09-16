package org.hl7.fhir.r4



import cats.data.NonEmptyList
import shapeless.{:+:, CNil}



abstract class Medication extends DomainResource



final object Medication
extends DomainResourceAttributes
{

  implicit def medicationResourceType[M <: Medication] =
    Resource.Type[M]("Medication")


  object Status extends CodedEnum {

    type Status = Value    

    val Active         = Val("active","Active")
    val Inactive       = Val("inactive","Inactive")
    val EnteredInError = Val("entered-in-error","Entered in Error")

    implicit val format = json.formatCodedEnum(this)
  }


  trait status[C[_]] extends Medication {
    val status: C[Status.Value]
  }

  trait manufacturer[C[_]]{
    val manufaturer: C[Reference[Organization]]
  }

  trait form[C[+_]]{
    val form: C[CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]]
  }


//  abstract class IngredientElement[I: Ingredient.ValidItem] extends BackboneElement[Zero,Many]
  abstract class IngredientElement[I: Ingredient.ValidItem] extends BackboneElement[Many]
  {
    this: Ingredient.item[I] =>
  }

  object Ingredient
  extends BackboneElementAttributes
  {

    type ValidItem[I] = I IsIn (Ref[Medication] :+: Ref[Substance] :+: CodeableConcept :+: CNil)

    sealed trait item[+X]

    trait itemCodeableConcept[+T <: CodeableConcept] extends item[T]{
      val itemCodeableConcept: T
    }

    trait itemReference[+I <: Resource] extends item[Reference[I]]{
      val itemReference: Reference[I]
    }

    trait isActive[C[_]]{
      this: IngredientElement[_] =>
      val isActive: C[Boolean]
    }

  }

  trait ingredient[+I <: IngredientElement[_], C[+_]] extends Medication {
    val ingredient: C[List[I]]
  }

  trait ingredientNel[+I <: IngredientElement[_]] extends Medication {
    val ingredient: NonEmptyList[I]
  }

}
