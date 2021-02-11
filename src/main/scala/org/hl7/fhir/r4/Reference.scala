package org.hl7.fhir.r4



import java.net.URI


import cats.data.Ior

import play.api.libs.json._



sealed trait Reference[+R <: Resource]


final case class LiteralReference[+R <: Resource]
(
  reference: String
)
extends Reference[R]


final case class LogicalReference[+R <: Resource]
(
  identifier: Identifier
)
extends Reference[R]


final case class BasicReference[+R <: Resource](
  refIorId: Ior[String,Identifier]
)
extends Reference[R]
{
  val reference  = refIorId.onlyLeft
  val identifier = refIorId.onlyRight
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
    LiteralReference(r)
  }


  // Logical Reference factory function
  def apply[R <: Resource](
    id: Identifier
  ): LogicalReference[R] = {
    LogicalReference(id)
  }


  // Logical and/or Literal Reference factory function
  def apply[R <: Resource](
    refIorId: Ior[String,Identifier]
  ): Reference[R] = {
    BasicReference(refIorId)
  }


  def contained[R <: Resource: Resource.HasId](
    r: R
  ): LiteralReference[R] = {

    import scala.language.reflectiveCalls

    val id = r.asInstanceOf[{ val id: String }].id

    LiteralReference(s"#$id")
  }



  implicit def formatLiteralRef[R <: Resource](
    implicit rs: Resource.Type[R]
  ): Format[LiteralReference[R]] =
    Format(
      Json.reads[LiteralReference[R]]
        .composeWith(
          Reads(
            js =>
              (js \ "type").validate[String]
                .filter(JsError(s"Invalid 'type' on Reference; expected ${rs.name}"))(_ == rs.name)
                .map(_ => js)
          )
        ),
      Json.writes[LiteralReference[R]]
        .transform(
          (js: JsValue) => js.as[JsObject] + ("type" -> JsString(rs.name))
        )
    )


  implicit def formatLogicalRef[R <: Resource](
    implicit rs: Resource.Type[R]
  ): Format[LogicalReference[R]] =
    Format(
      Json.reads[LogicalReference[R]]
        .composeWith(
          Reads(
            js =>
              (js \ "type").validate[String]
                .filter(JsError(s"Invalid 'type' on Reference; expected ${rs.name}"))(_ == rs.name)
                .map(_ => js)
          )
        ),
      Json.writes[LogicalReference[R]]
        .transform(
          (js: JsValue) => js.as[JsObject] + ("type" -> JsString(rs.name))
        )
    )
  

  import play.api.libs.functional.syntax._

  implicit def formatBasicRef[R <: Resource](
    implicit rs: Resource.Type[R]
  ): Format[BasicReference[R]] =
    Format(
      (
        (JsPath \ "reference").readNullable[String] and
        (JsPath \ "identifier").readNullable[Identifier]
      )(
        (ref,id) => Ior.fromOptions(ref,id)
      )
      .filter(JsonValidationError("At least one of 'reference' or 'identifier' must be defined"))(_.isDefined)
      .map(_.get)
      .map(BasicReference[R](_))
      .composeWith(
        Reads(
          js =>
            (js \ "type").validate[String]
              .filter(JsError(s"Invalid 'type' on Reference; expected ${rs.name}"))(_ == rs.name)
              .map(_ => js)
        )
      ),
      (
        (JsPath \ "reference").writeNullable[String] and
        (JsPath \ "identifier").writeNullable[Identifier]
      )(
        (ref: BasicReference[R]) => (ref.reference,ref.identifier)
      )  
      .transform(
        (js: JsValue) => js.as[JsObject] + ("type" -> JsString(rs.name))
      )
    )
    

}




/*
sealed trait Reference[+R <: Resource]
{
  val `type`: String
  val reference: Option[String]
  val identifier: Option[Identifier]
}


object Reference
{
 
  private case class ReferenceImpl[+R <: Resource]
  (
    refIorId: Ior[String,Identifier],
    `type`: String
  ) extends Reference[R]
  {
    val reference  = refIorId.onlyLeft
    val identifier = refIorId.onlyRight
  }


  // Literal Reference factory function
  def apply[R <: Resource](
    ref: String
  )(
    implicit rs: Resource.Type[R]
  ): Reference[R] = {
    val t = rs.name
    val r = if (ref.startsWith(t)) ref
            else t + "/" + ref
    ReferenceImpl(Ior.Left(r),t)
  }


  def contained[R <: Resource: Resource.HasId](
    r: R
  )(
    implicit rs: Resource.Type[R]
  ): Reference[R] = {

    import scala.language.reflectiveCalls

    val id = r.asInstanceOf[{ val id: String }].id

    ReferenceImpl(Ior.Left(s"#$id"),rs.name)
  }


  // Logical Reference factory function
  def apply[R <: Resource](
    id: Identifier
  )(
    implicit rs: Resource.Type[R]
  ): Reference[R] = {
    ReferenceImpl(Ior.Right(id),rs.name)
  }


  // Logical & Literal Reference factory function
  def apply[R <: Resource](
    refIorId: Ior[String,Identifier]
  )(
    implicit rs: Resource.Type[R]
  ): Reference[R] = {
    ReferenceImpl(refIorId,rs.name)
  }


  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit def writesReference[R <: Resource] =
    (
      (JsPath \ "type").write[String] and
      (JsPath \ "reference").writeNullable[String] and
      (JsPath \ "identifier").writeNullable[Identifier]
    )(
      (ref: Reference[R]) => (ref.`type`,ref.reference,ref.identifier)
    )  

  implicit def readsReference[R <: Resource: Resource.Type]: Reads[Reference[R]] =
    (
      (JsPath \ "reference").readNullable[String] and
      (JsPath \ "identifier").readNullable[Identifier]
    )(
      (ref,id) => Ior.fromOptions(ref,id)
    )
    .filter(JsonValidationError("At least one of 'reference' or 'identifier' must be defined"))(_.isDefined)
    .map(_.get)
    .map(Reference[R](_))

}
*/
