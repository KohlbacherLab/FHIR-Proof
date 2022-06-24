package org.hl7.fhir.r4


import java.time.temporal.Temporal
import java.net.URI

import cats.data.NonEmptyList

import play.api.libs.json._



class PositiveInt private (val value: Int) extends AnyVal
object PositiveInt
{

  def apply(i: Int): PositiveInt = {
    if (i > 0) new PositiveInt(i)
    else throw new IllegalArgumentException(s"Invalid PositiveInt value $i")
  }

  import scala.language.implicitConversions

  implicit def toInt(pos: PositiveInt): Int = pos.value

  implicit val format: Format[PositiveInt] =
    Format(
      Reads.of[Int].filter(JsonValidationError("Found non-positive Int value; expected PositiveInt"))(_ > 0)
        .map(PositiveInt(_)),
      Writes.of[Int].contramap(_.value)
    )

}


trait ExtensionSet {
  this: Product =>
}

trait Extensible
trait ExtensibleAttributes
{

  trait extension[+E <: Extension, C[+_]]{ 
    this: Extensible =>
    val extension: C[List[E]]
  }

  trait extensionNel[+E <: Extension]{ 
    this: Extensible =>
    val extension: NonEmptyList[E]
  }

  trait extensions[+E <: ExtensionSet, C[+_]]{ 
    this: Extensible =>
    val extension: C[E]
  }

}


trait ModifierExtensible
extends Extensible

trait ModifierExtensibleAttributes extends ExtensibleAttributes
{

  trait modifierExtension[+E <: Extension]{
    this: ModifierExtensible =>
    val modifierExtension: NonEmptyList[E]
  } 

  trait modifierExtensions[+E <: ExtensionSet, C[+_]]{ 
    this: ModifierExtensible =>
    val modifierExtension: E
  }
}


trait Element extends Extensible

trait ElementAttributes extends ExtensibleAttributes
{
  trait id[C[_]]{
    this: Element =>
    val id: String
  }
}



trait BackboneElement[Max <: MaxOccurrence] extends Element with ModifierExtensible


trait BackboneElementAttributes extends ElementAttributes with ModifierExtensibleAttributes




trait Period[T <: Temporal]

final case class OpenEndPeriod[T <: Temporal]
(
  start: T,
  end: Option[T] = None
) extends Period[T]

object OpenEndPeriod
{
  implicit def format[T <: Temporal: Format] =
    Json.format[OpenEndPeriod[T]]
}

final case class ClosedPeriod[T <: Temporal]
(
  start: T,
  end: T
) extends Period[T]

object ClosedPeriod
{
  implicit def format[T <: Temporal: Format] =
    Json.format[ClosedPeriod[T]]
}




case class Identifier
(
  value: String,
  system: Option[URI] = None,
  use: Option[Identifier.Use.Value] = None 
)


object Identifier
{

  object Use extends Enumeration
  {
    type Use = Value

    val Usual     = Value("usual")
    val Official  = Value("official")
    val Secondary = Value("secondary")
    val Temp      = Value("temp")
    val Old       = Value("old")

    implicit val format =
      Json.formatEnum(Use)
  }


  implicit def formatIdentifier =
    Json.format[Identifier]

}


@annotation.implicitNotFound("Couldn't find CodingSystem instance for ${C}")
sealed trait CodingSystem[C]{ val uri: URI }
object CodingSystem
{
  def apply[C](implicit sys: CodingSystem[C]): CodingSystem[C] = sys

  def apply[C](sys: URI): CodingSystem[C] = new CodingSystem[C]{ val uri = sys }
  def apply[C](sys: String): CodingSystem[C] = CodingSystem[C](URI.create(sys))
}


sealed abstract class Coding extends Element
{
  val code: String
}

object Coding extends ElementAttributes
{

  trait display[C[_]] {
    this: Coding =>
    val display: C[String]
  }

  trait version[C[_]] {
    this: Coding =>
    val display: C[String]
  }

  trait system[C[_]] {
    this: Coding =>
    val system: C[String]
  }

}

final case class CodingStatic[S: CodingSystem]
(
  code: String,
  display: Option[String],
  version: Option[String]
) 
extends Coding
   with Coding.display[Optional]
   with Coding.version[Optional]


object CodingStatic
{
/*
  def apply[C <: Coded](
    c: C
  )(
    implicit sys: CodingSystem[C]
  ): CodingStatic[C] =
    CodingStatic[C](c.code,c.display,None)

  def apply[C <: Enumeration](
    c: C#Value
  )(
    implicit sys: CodingSystem[C#Value]
  ): CodingStatic[C#Value] =
    CodingStatic[C#Value](c.asInstanceOf[Coded].code,c.asInstanceOf[Coded].display,None)
*/

  def apply[E <: Enumeration](
    c: E#Value,
    display: Option[String] = None
  )(
    implicit sys: CodingSystem[E#Value]
  ): CodingStatic[E#Value] =
    CodingStatic[E#Value](c.toString,display,None)


  import play.api.libs.functional.syntax._

  implicit def format[S](implicit system: CodingSystem[S]): Format[CodingStatic[S]] = {
    Format(
      Json.reads[CodingStatic[S]]
        .composeWith(
          Reads(js => (js \ "system").validate[URI].filter(JsError("Missing or invalid Coding.system; expected ${system.uri}"))(_ == system.uri).map(_ => js))
        ),
      Json.writes[CodingStatic[S]]
        .transform((js: JsValue) => js.as[JsObject] + ("system" -> JsString(system.uri.toString)))
    )
  }
}

final case class CodingDynamic
(
  code: String,
  display: Option[String],
  system: String,
  version: Option[String]
) 
extends Coding
   with Coding.display[Optional]
   with Coding.system[Required]
   with Coding.version[Optional]


object CodingDynamic
{
  implicit val format = Json.format[CodingDynamic]
}




sealed abstract class CodeableConcept extends Element

object CodeableConcept extends ElementAttributes
{

  trait coding[+C <: Coding, F[+_]] {
    this: CodeableConcept =>
    val coding: F[List[C]]
  }
  trait codingNel[+C <: Coding] {
    this: CodeableConcept =>
    val coding: NonEmptyList[C]
  }
  trait text[C[_]] {
    this: CodeableConcept =>
    val text: C[String]
  }

}


final case class CodeableConceptStatic[S]
(
  coding: NonEmptyList[CodingStatic[S]],
  text: Option[String]
)
extends CodeableConcept
   with CodeableConcept.codingNel[CodingStatic[S]]
   with CodeableConcept.text[Optional]


object CodeableConceptStatic
{

  def apply[S](coding: CodingStatic[S]): CodeableConceptStatic[S] =
    CodeableConceptStatic(NonEmptyList.one(coding),None)

//  def apply[C <: Coded](c: C)(implicit sys: CodingSystem[C]): CodeableConceptStatic[C] =
//    CodeableConceptStatic(CodingStatic[C](c))


  import json.formatNel

  implicit def format[S: CodingSystem](
    implicit f: Format[CodingStatic[S]]
  ): Format[CodeableConceptStatic[S]] = Json.format[CodeableConceptStatic[S]]

}


final case class CodeableConceptDynamic
(
  coding: NonEmptyList[CodingDynamic],
  text: Option[String]
)
extends CodeableConcept
   with CodeableConcept.codingNel[CodingDynamic]
   with CodeableConcept.text[Optional]

object CodeableConceptDynamic
{

  def apply(coding: CodingDynamic): CodeableConceptDynamic =
    CodeableConceptDynamic(NonEmptyList.one(coding),None)


  import json.formatNel

  implicit val format = Json.format[CodeableConceptDynamic]
}

/*
abstract class Coding[S: Coding.System] extends Element

object Coding extends ElementAttributes
{

  trait code[C[_]] {
    this: Coding[_] =>
    val code: C[String]
  }

  trait display[C[_]] {
    this: Coding[_] =>
    val display: C[String]
  }

  trait version[C[_]] {
    this: Coding[_] =>
    val display: C[String]
  }

  trait system[C[_]] {
    this: Coding[_] =>
    val system: C[String]
  }


  @annotation.implicitNotFound("Couldn't find Coding.System instance for ${C}")
  sealed trait System[C]{ val uri: URI }
  object System
  {
    def apply[C](implicit sys: System[C]): System[C] = sys

    def apply[C](sys: URI): System[C] = new System[C]{ val uri = sys }
    def apply[C](sys: String): System[C] = System[C](URI.create(sys))
  }

}


final case class BasicCoding[S: Coding.System]
(
  code: String,
  display: Option[String] = None,
  version: Option[String] = None,
)
extends Coding
   with Coding.code[Required]
   with Coding.display[Optional]
   with Coding.version[Optional]


object BasicCoding
{

  def apply[C <: Coded](
    c: C
  )(
    implicit sys: Coding.System[C]
  ): BasicCoding[C] =
    BasicCoding[C](c.code,c.display,None)


  def apply[C <: Enumeration](
    c: C#Value
  )(
    implicit sys: Coding.System[C#Value]
  ): BasicCoding[C#Value] =
    BasicCoding[C#Value](c.asInstanceOf[Coded].code,c.asInstanceOf[Coded].display,None)

  def apply[E <: Enumeration](
    c: E#Value,
    display: Option[String]
  )(
    implicit sys: Coding.System[E#Value]
  ): BasicCoding[E#Value] =
    BasicCoding[E#Value](c.toString,display,None)


  import play.api.libs.functional.syntax._

  implicit def format[S](implicit system: Coding.System[S]): Format[BasicCoding[S]] = {

    val read: Reads[BasicCoding[S]] =
      (
        (JsPath \ "code").read[String] and
        (JsPath \ "display").readNullable[String] and
        (JsPath \ "version").readNullable[String]
      )(BasicCoding.apply[S] _)

    val write: Writes[BasicCoding[S]] =
      (
        (JsPath \ "code").write[String] and
        (JsPath \ "display").writeNullable[String] and
        (JsPath \ "version").writeNullable[String]
      )(unlift(BasicCoding.unapply[S]))

    Format[BasicCoding[S]](
      Reads( js =>
        (js \ "system").validate[URI]
          .filter(JsError(s"Invalid Coding.system; expected ${system.uri}"))(_ == system.uri)
          .flatMap(sys => read.reads(js))
      ),
      write.transform(_.as[JsObject] + ("system"  -> JsString(system.uri.toString)))
    )
  }

}



abstract class CodeableConcept extends Element

object CodeableConcept extends ElementAttributes
{

  trait coding[+C <: Coding[_], F[+_]] {
    this: CodeableConcept =>
    val coding: F[List[C]]
  }
  trait codingNel[+C <: Coding[_]] {
    this: CodeableConcept =>
    val coding: NonEmptyList[C]
  }

  trait text[C[_]] {
    this: CodeableConcept =>
    val text: C[String]
  }

}


final case class BasicCodeableConcept[S]
(
  coding: NonEmptyList[BasicCoding[S]],
  text: Option[String]
)
extends CodeableConcept
   with CodeableConcept.codingNel[BasicCoding[S]]
   with CodeableConcept.text[Optional]


object BasicCodeableConcept
{

  def apply[S](coding: BasicCoding[S]): BasicCodeableConcept[S] =
    BasicCodeableConcept(NonEmptyList.one(coding),None)

  def apply[C <: Coded](c: C)(implicit sys: Coding.System[C]): BasicCodeableConcept[C] =
    BasicCodeableConcept(BasicCoding[C](c))


  import json.formatNel

  implicit def format[S: Coding.System](
    implicit f: Format[BasicCoding[S]]
  ): Format[BasicCodeableConcept[S]] = Json.format[BasicCodeableConcept[S]]

}
*/
