package org.hl7.fhir.r4



import java.net.{URI,URL}

import java.time._

import java.util.UUID


trait ValidValues[Vs <: shapeless.Coproduct]
{
  type ValidValue[T] = T IsIn Vs
}


trait CanHaveValue[Vs <: shapeless.Coproduct] extends ValidValues[Vs]
{

  sealed trait value[+X]

  trait valueBoolean[C[_]] extends value[Boolean]{
    val valueBoolean: C[Boolean]
  }

  trait valueString[C[_]] extends value[String]{
    val valueString: C[String]
  }

  trait valueInteger[C[_]] extends value[Int]{
    val valueInteger: C[Int]
  }

  trait valueLong[C[_]] extends value[Long]{
    val valueInteger: C[Long]
  }

  trait valuePositiveInt[C[_]] extends value[PositiveInt]{
    val valuePositiveInt: C[PositiveInt]
  }

  trait valueUnsignedInt[C[_]] extends value[Int]{
    val valueUnsignedInt: C[Int]
  }

  trait valueFloat[C[_]] extends value[Float]{
    val valueDecimal: C[Float]
  }

  trait valueDouble[C[_]] extends value[Double]{
    val valueDecimal: C[Double]
  }

  trait valueDate[C[_]] extends value[LocalDate]{
    val valueDate: C[LocalDate]
  }

  trait valueDateTime[C[_]] extends value[LocalDateTime]{
    val valueDateTime: C[LocalDateTime]
  }
  trait valueOffsetDateTime[C[_]] extends value[OffsetDateTime]{
    val valueDateTime: C[OffsetDateTime]
  }

  trait valueInstant[C[_]] extends value[Instant]{
    val valueInstant: C[Instant]
  }

  trait valueUri[C[_]] extends value[URI]{
    val valueUri: C[URI]
  }

  trait valueUuid[C[_]] extends value[UUID]{
    val valueUri: C[UUID]
  }

  trait valueUrl[C[_]] extends value[URL]{
    val valueUrl: C[URL]
  }

  trait valueQuantity[+Q <: Quantity,F[+_]] extends value[Q]{
    val valueQuantity: F[Q]
  }

  trait valueRange[+R <: Range,F[+_]] extends value[R]{
    val valueRange: F[R]
  }

  trait valueRatio[+R <: Ratio,F[+_]] extends value[R]{
    val valueRatio: F[R]
  }

  trait valueCoding[+C <: Coding,F[+_]] extends value[C]{
    val valueCoding: F[C]
  }

  trait valueCodeableConcept[+CC <: CodeableConcept,F[+_]] extends value[CC]{
    val valueCodeableConcept: F[CC]
  }


}


@annotation.implicitNotFound("${T} is not an admissible valueElement")
trait ValueElement[T]
{
  val name: String
}

object ValueElement
{

  private def apply[T](n: String): ValueElement[T] =
    new ValueElement[T]{ val name = n }

  def apply[T](implicit v: ValueElement[T]): ValueElement[T] = v


  implicit val valueBoolean            = ValueElement[Boolean]("valueBoolean")
  implicit val valueString             = ValueElement[String]("valueString")
  implicit val valueInteger            = ValueElement[Int]("valueInteger")
  implicit val valuePositiveInt        = ValueElement[PositiveInt]("valuePositiveInt")
  implicit val valueDouble             = ValueElement[Double]("valueDecimal")
  implicit val valueFloat              = ValueElement[Float]("valueDecimal")
  implicit val valueDate               = ValueElement[LocalDate]("valueDate")
  implicit val valueDateTime           = ValueElement[LocalDateTime]("valueDateTime")
  implicit val valueOffsetDateTime     = ValueElement[OffsetDateTime]("valueDateTime")
  implicit val valueTime               = ValueElement[LocalTime]("valueTime")
  implicit val valueIdentifier         = ValueElement[Identifier]("valueIdentifier")
  implicit val valueUri                = ValueElement[URI]("valueUri")
  implicit val valueUrl                = ValueElement[URL]("valueUrl")
  implicit val valueUuid               = ValueElement[UUID]("valueUuid")

  implicit def valueQuantity[Q <: Quantity] = ValueElement[Q]("valueQuantity")

  implicit def valueRange[R <: Range] = ValueElement[R]("valueRange")

  implicit def valueRatio[R <: Ratio] = ValueElement[R]("valueRatio")

  implicit def valueCoding[C <: Coding] = ValueElement[C]("valueCoding")

  implicit def valueCodeableConcept[CC <: CodeableConcept] = ValueElement[CC]("valueCodeableConcept")

  implicit def valueReference[R <: Reference[_]] = ValueElement[R]("valueReference")

  //TODO: other valueElement types

}
