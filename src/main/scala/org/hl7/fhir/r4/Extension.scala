package org.hl7.fhir.r4



import java.net.{URI,URL}
import java.util.UUID
import java.time.temporal.Temporal

import shapeless.{
  ::, HList, HNil,
  Lazy, Generic,
  :+:, CNil
}

import play.api.libs.json._


trait Extension


abstract class SimpleExtension[+V: Extension.ValidValue] extends Extension
{
  val value: V
}

object Extension
extends ValidValues[
  String :+:
  Boolean :+:
  Int :+:
  PositiveInt :+:
  Long :+:
  Float :+:
  Double :+:
  URI :+:
  URL :+:
  UUID :+:
  Temporal :+:
  Coding[_] :+:
  CodeableConcept :+: 
  CNil
] //TODO: complete list
{


  @annotation.implicitNotFound(
  "Couldn't find implicit Extension.Url for ${E}. Define it or ensure it is in scope."
  )
  sealed trait Url[E]{ val value: URI }
  object Url
  {
    def apply[E: IsValidExtension](implicit url: Url[E]) = url

    def apply[E: IsValidExtension](uri: URI): Url[E] =
      new Url[E]{ val value = uri }

    def apply[E: IsValidExtension](uri: String): Url[E] =
      new Url[E]{ val value = URI.create(uri) }
  }


  @annotation.implicitNotFound(
  """${E} is not a valid Extension. Extensions must be sub-classes of SimpleExtension or Products of Extension types"""
  )
  sealed trait IsValidExtension[E]
  object IsValidExtension
  {
    def apply[E](implicit ext: IsValidExtension[E]) = ext

    implicit def simpleExtension[E <: SimpleExtension[_]]: IsValidExtension[E] =
      new IsValidExtension[E]{}

    implicit def optionalExtension[E: IsValidExtension]: IsValidExtension[Option[E]] =
      new IsValidExtension[Option[E]]{}

    implicit def extensions[E: IsValidExtension, C[X] <: Iterable[X]]: IsValidExtension[C[E]] =
      new IsValidExtension[C[E]]{}

    implicit def compositeExtension[E <: Product, R](
      implicit
      gen: Generic.Aux[E,R],
      ext: IsValidExtension[R]
    ): IsValidExtension[E] = new IsValidExtension[E]{}

    implicit def extensionHList[H, T <: HList](
      implicit allExt: ForAll[H :: T, IsValidExtension]
    ): IsValidExtension[H :: T] = new IsValidExtension[H :: T]{}

  }


}
