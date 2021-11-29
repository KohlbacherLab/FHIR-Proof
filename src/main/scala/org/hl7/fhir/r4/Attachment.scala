package org.hl7.fhir.r4


import java.net.URL
import java.time.temporal.Temporal

import cats.data.NonEmptyList


abstract class Attachment extends Element

object Attachment extends ElementAttributes
{

  //TODO: contentType
  //TODO: language


  trait url[C[_]]{
    this: Attachment =>
    val url: C[URL]
  }

  trait data[C[_]]{
    this: Attachment =>
    val data: C[Array[Byte]]
  }

  trait hash[C[_]]{
    this: Attachment =>
    val hash: C[Array[Byte]]
  }

  trait size[C[_]]{
    this: Attachment =>
    val size: C[Long]
  }

  trait title[C[_]]{
    this: Attachment =>
    val titel: C[String]
  }

  trait creation[T <: Temporal, C[_]]{
    this: Attachment =>
    val creation: C[T]
  }

}

