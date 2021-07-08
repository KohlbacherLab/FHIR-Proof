package org.hl7.fhir.r4


import java.net.URI

import shapeless.{:+:, CNil}



abstract class MessageHeader extends DomainResource
{
  this: MessageHeader.event[_] =>
}


object MessageHeader
extends DomainResourceAttributes
{

  implicit def messageHeaderResourceType[M <: MessageHeader] =
    Resource.Type[M]("MessageHeader")


  type ValidEvent[T] = T IsIn (Coding :+: URI :+: CNil)
//  type ValidEvent[T] = T IsIn (Coding[_] :+: URI :+: CNil)


  sealed trait event[+X]

//  trait eventCoding[T <: Coding[_]] extends event[T]{
  trait eventCoding[T <: Coding] extends event[T]{
    this: MessageHeader =>
    val eventCoding: T
  }

  trait eventUri extends event[URI]{
    this: MessageHeader =>
    val eventUri: URI
  }



}
