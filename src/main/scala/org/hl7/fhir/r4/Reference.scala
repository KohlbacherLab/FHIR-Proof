package org.hl7.fhir.r4



import java.net.URI


import cats.data.Ior

import play.api.libs.json._



sealed trait Reference[+R <: Resource]
{
  val `type`: String
}

final case class LiteralReference[+R <: Resource] private
(
  reference: String,
  `type`: String
)
extends Reference[R]


final case class LogicalReference[+R <: Resource] private
(
  identifier: Identifier,
  `type`: String
)
extends Reference[R]


final case class BasicReference[+R <: Resource] private
(
  refIorId: String Ior Identifier,
  `type`: String
)
extends Reference[R]
{
  val reference  = refIorId.onlyLeft
  val identifier = refIorId.onlyRight
}

//TODO: 'Embedded' Reference, containing the referenced resource


object LiteralReference
{

  // Literal Reference factory function
  def apply[R <: Resource](
    ref: String
  )(
    implicit rs: Resource.Type[R]
  ): LiteralReference[R] = {
    val t = rs.name
    val r = if (ref.startsWith(t)) ref
            else t + "/" + ref
    LiteralReference(r,t)
  }

}


object LogicalReference
{

  // Logical Reference factory function
  def apply[R <: Resource](
    id: Identifier
  )(
    implicit rs: Resource.Type[R]
  ): LogicalReference[R] = {
    LogicalReference(id,rs.name)
  }

}


object Reference
{

  // Literal Reference factory function
  def apply[R <: Resource](
    ref: String
  )(
    implicit rs: Resource.Type[R]
  ): LiteralReference[R] = {
    val t = rs.name
    val r = if (ref.startsWith(t)) ref
            else t + "/" + ref
    LiteralReference(r,t)
  }


  // Logical Reference factory function
  def apply[R <: Resource](
    id: Identifier
  )(
    implicit rs: Resource.Type[R]
  ): LogicalReference[R] = {
    LogicalReference(id,rs.name)
  }


  // Logical and/or Literal Reference factory function
  def apply[R <: Resource](
    refIorId: Ior[String,Identifier]
  )(
    implicit rs: Resource.Type[R]
  ): Reference[R] = {
    BasicReference(refIorId,rs.name)
  }


  def contained[R <: Resource: Resource.HasId](
    r: R
  )(
    implicit rs: Resource.Type[R]
  ): LiteralReference[R] = {

    import scala.language.reflectiveCalls

    val id = r.asInstanceOf[{ val id: String }].id

    LiteralReference(s"#$id",rs.name)
  }



  implicit def formatLiteralRef[R <: Resource] =
    Json.format[LiteralReference[R]]

  implicit def formatLogicalRef[R <: Resource] =
    Json.format[LogicalReference[R]]
  

  import play.api.libs.functional.syntax._

  implicit def formatBasicRef[R <: Resource]: Format[BasicReference[R]] =
    Format(
      (
        (JsPath \ "reference").readNullable[String] and
        (JsPath \ "identifier").readNullable[Identifier] and
        (JsPath \ "type").read[String]
      )(
        (ref,id,t) => (Ior.fromOptions(ref,id),t)
      )
      .filter(JsonValidationError("At least one of 'reference' or 'identifier' must be defined"))(_._1.isDefined)
      .map { case (ior,t) => BasicReference[R](ior.get,t) },

      (
        (JsPath \ "reference").writeNullable[String] and
        (JsPath \ "identifier").writeNullable[Identifier] and
        (JsPath \ "type").write[String]
      )(
        (ref: BasicReference[R]) => (ref.reference,ref.identifier,ref.`type`)
      )  
    )
    
}

