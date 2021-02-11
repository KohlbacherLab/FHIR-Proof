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

  implicit def format(
    implicit
    read: Reads[Int],
    write: Writes[Int]
  ): Format[PositiveInt] =
    Format[PositiveInt](
      read.filter(JsonValidationError("Found non-positive Int value; expected PositiveInt"))(_ > 0)
        .map(PositiveInt(_)),
      write.contramap(_.value)
    )
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

  trait extensions[+E <: Product, C[+_]]{ 
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

  trait modifierExtensions[+E <: Product]{ 
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

  object Use extends CodedEnum
  {
    type Use = Value
    val Usual     = Val("usual","Usual")
    val Official  = Val("official","Official")
    val Secondary = Val("secondary","Secondary")
    val Temp      = Val("temp","Temp")
    val Old       = Val("old","Old")
  }

  implicit val formatUse =
    json.formatCodedEnum(Use)

  implicit def formatIdentifier =
    Json.format[Identifier]

}



trait Coded
{
  val code: String
  val display: Option[String]
}

object Coded
{
  def mapping[C <: Coded](cs: C*): Map[C,String] = 
    cs.map(c => (c,c.code)).toMap
}

abstract class CodedEnum extends Enumeration
{

  protected case class Val(
    code: String,
    display: Option[String]
  ) extends super.Val with Coded

  object Val
  {
    def apply(code: String, display: String): Val = Val(code,Some(display))
  }

}



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


  def apply[C <: CodedEnum](
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

