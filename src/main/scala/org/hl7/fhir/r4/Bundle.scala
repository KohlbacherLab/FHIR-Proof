package org.hl7.fhir.r4


import java.net.URI
import java.time.Instant

import cats.data.NonEmptyList

import shapeless.{:+:, CNil, Generic, HList, ::, HNil, Lazy}
import shapeless.{HList}

import play.api.libs.json._


sealed abstract class Bundle extends Resource
{
  this: Bundle.entry[_] =>
}


object Bundle
{

  implicit def bundleResourceType[B <: Bundle] =
    Resource.Type[B]("Bundle")


  sealed trait Type[B <: Bundle]{ val value: String }
  object Type {

    def apply[B <: Bundle](implicit t: Type[B]) = t

    implicit def collection[C <: Collection] =
      new Type[C]{ val value = "collection" }
    
    implicit def history[H <: History[_]] =
      new Type[H]{ val value = "history" }

    //TODO: other bundle types
  }



  trait identifier[C[_]]{
    this: Bundle =>
    val identifier: C[Identifier]
  }

  trait timestamp[C[_]]{
    this: Bundle =>
    val timestamp: C[Instant]
  }


  sealed trait HasTotal
  {
    def total: Int 
  }


  abstract class EntryElement extends BackboneElement[Many]

  object Entry extends BackboneElementAttributes
  {
    trait fullUrl{
      this: EntryElement =>
      val fullUrl: URI
    }
    trait resource[+R <: Resource]{
      this: EntryElement =>
      val resource: R
    }
  }

  case class EntryOf[+R <: Resource](
    resource: R
  ) 
  extends EntryElement
     with Entry.resource[R]


  object EntryOf
  {

//    implicit def format[R <: Resource: Format]: Format[EntryOf[R]] = {
//      Json.format[EntryOf[R]]
//    }

    import FHIRJson._

    implicit def format[R <: Resource](
      implicit
      fhir: FHIRFormat[R]
    ): Format[EntryOf[R]] = {
      implicit val f: Format[R] = fhir
      Json.format[EntryOf[R]]
    }

  }


  trait EntrySet {
    this: Product =>
  }


  trait entry[+E <: EntrySet]{
    this: Bundle =>
    val entry: E
  }


  abstract class Collection extends Bundle
  {
    this: entry[_] =>
  }



  abstract class History[+R <: Resource]
  extends Bundle
     with Bundle.entry[History.Entries[EntryElement with Entry.resource[R]]]
     with HasTotal
  {
    def total = this.entry.list.size
  }

  object History
  {

    case class Entries[+E <: EntryElement with Entry.resource[_]](
      list: List[E]
    ) extends EntrySet

    object Entries
    {

      implicit def format[E <: EntryElement with Entry.resource[_]: Format] =
        Format[Entries[E]](
          Reads(_.validate[List[E]].map(Entries(_))),
          Writes(es => Json.toJson(es.list))
        )

    }

  }




/*
  abstract class SearchSet extends Bundle with HasTotal
  {
    this: entry[_] =>
    val total: Int
  }
*/


}

