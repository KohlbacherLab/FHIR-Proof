package org.hl7.fhir.r4


import java.time.Instant
import java.net.URI

import shapeless.{HList, ::, HNil, Generic, Lazy}
import cats.data.NonEmptyList
import play.api.libs.json._



abstract class Resource
object Resource
{

  case class Id(value: String) extends AnyVal


  @annotation.implicitNotFound(
  "Couldn't find implicit value for Resource.Type[${R}]. Define it or ensure it is in scope"
  )
  sealed trait Type[R <: Resource]{ val name: String }
  object Type
  {
    implicit def apply[R <: Resource](implicit r: Lazy[Type[R]]) = r.value
   
    def apply[R <: Resource](n: String) =
      new Type[R]{ val name = n }  
  }


  @annotation.implicitNotFound("${R} doesn't have member 'val id: String' as required")
  sealed trait HasId[R]
  object HasId
  {
    def apply[R](implicit id: HasId[R]) = id
    implicit def withId[R <: { val id: String }]: HasId[R] =
      new HasId[R]{} 
  }

}


trait ResourceAttributes
{
  trait id[C[_]]{
    this: Resource =>
    val id: String
  }

  trait implicitRules[C[_]]{
    this: Resource =>
    val implicitRules: URI
  }
}


case class Meta
(
  versionId: Option[String] = None,
  lastUpdate: Option[Instant] = None,
  source: Option[URI] = None 
)

object Meta
{

  @annotation.implicitNotFound(
  "Couldn't find implicit value for Profiles[${R}]. Define one or ensure it is in scope."
  )
  trait Profiles[R <: Resource]{
    val list: List[URI]
  }

  object Profiles
  {
    def apply[R <: Resource](implicit ps: Profiles[R]) = ps

    def apply[R <: Resource](uri: URI): Profiles[R] =
      new Profiles[R]{ val list = List(uri) } 

    def apply[R <: Resource](uris: Seq[String]): Profiles[R] =
      new Profiles[R]{ val list = uris.map(URI.create).toList } 


    def apply[R <: Resource](uri: String, uris: String*): Profiles[R] =
      apply[R](uri +: uris)

  }

}



abstract class DomainResource
extends Resource
with ModifierExtensible

trait DomainResourceAttributes
extends ResourceAttributes
with ModifierExtensibleAttributes
{

  trait identifier[C[_]]{
    this: DomainResource =>
    val identifier: C[List[Identifier]]
  }

  trait identifierNel{
    this: DomainResource =>
    val identifier: NonEmptyList[Identifier]
  }

  trait contained[+CRs <: Product]{
    this: DomainResource =>
    val contained: CRs
  }

}

final object DomainResource
{


  @annotation.implicitNotFound(
  """${R} is not a valid contained Resource type.
  Only Resource type with member 'val id: String', Iterable[_] and NonEmptyList[_] are allowed"""
  )
  sealed trait IsContainedResource[R]
  object IsContainedResource
  {
    def apply[R](implicit c: IsContainedResource[R]) = c

    implicit def resourceWithId[R <: Resource: Resource.HasId]: IsContainedResource[R] =
      new IsContainedResource[R]{}

    implicit def resourceWithIdIterable[R <: Resource: Resource.HasId, C[X] <: Iterable[X]]: IsContainedResource[C[R]] =
      new IsContainedResource[C[R]]{}

    implicit def resourceWithIdNel[R <: Resource: Resource.HasId]: IsContainedResource[NonEmptyList[R]] =
      new IsContainedResource[NonEmptyList[R]]{}

    implicit def optContainedResource[R <: Resource: IsContainedResource]: IsContainedResource[Option[R]] =
      new IsContainedResource[Option[R]]{}
  }

}


