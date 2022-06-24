package org.hl7.fhir.r4


import play.api.libs.json.Json


abstract class Dosage extends BackboneElement[Many]

object Dosage extends BackboneElementAttributes
{


  abstract class DoseAndRateElement extends Element

  object DoseAndRate extends ElementAttributes
  {

     final object Type extends Enumeration
     {
       val Calculated = Value("calculated")
       val Ordered    = Value("ordered")

        implicit val system = CodingSystem[Type.Value]("http://hl7.org/fhir/ValueSet/dose-rate-type")

        implicit val format = Json.formatEnum(this) 
     }

    trait `type`[C[+_]]{ 
      this: DoseAndRateElement =>
      val `type`: C[CodeableConcept with CodeableConcept.codingNel[CodingStatic[Type.Value]]]
    }


    sealed trait dose[+X]

    trait doseRange[+R <: Range, C[+_]] extends dose[R]{
      this: DoseAndRateElement =>
      val doseRange: C[R]
    }

    trait doseQuantity[+Q <: Quantity, C[+_]] extends dose[Q]{
      this: DoseAndRateElement =>
      val doseQuantity: C[Q]
    }


    sealed trait rate[+X]

    trait rateRatio[+R <: Ratio, C[+_]] extends rate[R]{
      this: DoseAndRateElement =>
      val rateRatio: C[R]
    }

    trait rateRange[+R <: Range, C[+_]] extends rate[R]{
      this: DoseAndRateElement =>
      val rateRange: C[R]
    }

    trait rateQuantity[+Q <: Quantity, C[+_]] extends rate[Q]{
      this: DoseAndRateElement =>
      val rateQuantity: C[Q]
    }


  }

  trait doseAndRate[+D <: DoseAndRateElement, C[+_]]{
    this: Dosage =>
    val doseAndRate: C[D]
  } 


}
