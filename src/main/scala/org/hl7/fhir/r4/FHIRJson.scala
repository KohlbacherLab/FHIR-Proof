package org.hl7.fhir.r4



import java.net.URI

import play.api.libs.json._

import shapeless.{<:!<}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


object FHIRJson
{

  implicit class ChainingOps[T](val t: T) extends AnyVal
  {
    def |[U](f: T => U): U = f(t)
  }


/*
  private val RESOURCE_TYPE = "resourceType"


  private def addProfiles(
    js: JsObject,
    uris: List[URI]
  ): JsObject = {
    (js \ "meta").asOpt[JsObject].getOrElse(JsObject.empty) |
       (meta => meta + ("profile" -> Json.toJson(uris))) |
       (meta => js + ("meta" -> meta))
    }



  def format[R <: Resource](
    implicit
    uneq: R <:!< HasLOINCCode,
    rs: Resource.Type[R],
    pfs: Meta.Profiles[R] 
  ): Format[R] = macro formatImpl[R]


  def formatImpl[R <: Resource](c: Context)(
//    uneq: c.Expr[R <:!< HasLOINCCode],
//    rs: c.Expr[Resource.Type[R]],
//    pfs: c.Expr[Meta.Profiles[R]] 
    uneq: c.Tree,
    rs: c.Tree,
    pfs: c.Tree
  ): c.Tree = {

    import c.universe._

    q"""
    Format(
      Json.reads[R].composeWith(
        Reads(
          js =>
            for {
              rsType <- (js \ "resourceType").validate[String]
                          .filter(JsError(s"Invalid or missing attribute '$$RESOURCE_TYPE'; expected $${rs.name}"))(_ == rs.name)
              profile <- (js \ "meta" \ "profile").validate[List[URI]]
                          .filter(JsError(s"Invalid or unexpected profiles; expected $${pfs.list}"))(l => pfs.list forall l.contains)
            } yield js
        )
      ),
      Json.writes[R].transform(
        js =>
          js.as[JsObject] |
            (_ + (RESOURCE_TYPE -> JsString(rs.name))) |
            (addProfiles(_,pfs.list))
 
      )
    )
   """
  }
*/ 




  @annotation.implicitNotFound(
  """Couldn't derive FHIRFormat for ${T}.
  Ensure Format[${T}], Resource.Type[${T}] and Meta.Profiles[${T}] are in scope"""
  )
  sealed trait FHIRFormat[T]
  {
    def reads(js: JsValue): JsResult[T]
    def writes(t: T): JsValue
  }

  object FHIRFormat
  {

    def apply[T](implicit f: FHIRFormat[T]) = f

    import scala.language.implicitConversions

    implicit def toFormat[T](f: FHIRFormat[T]): Format[T] =
      Format[T](
        Reads(f.reads),
        Writes(f.writes)
      )


    private def addProfiles(
      js: JsObject,
      uris: List[URI]
    ): JsObject = {
      (js \ "meta").asOpt[JsObject].getOrElse(JsObject.empty) |
        (meta => meta + ("profile" -> Json.toJson(uris))) |
          (meta => js + ("meta" -> meta))
    }


    implicit def bundleFormat[R <: Bundle](
      implicit
      f: Format[R],
      rs: Resource.Type[R],
      bt: Bundle.Type[R],
      ps: Meta.Profiles[R] 
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {
          for {
            rsType <- (js \ "resourceType").validate[String]
                        .filter(JsError(s"Invalid or missing attribute 'resourceType'; expected ${rs.name}"))(_ == rs.name)
           
            bdlType <- (js \ "type").validate[String]
                         .filter(JsError("Invalid or missing Bundle 'type'"))(_ == bt.value)
           
            profile <- (js \ "meta" \ "profile").validate[List[URI]]
                         .filter(JsError(s"Invalid or unexpected profiles; expected ${ps.list}"))(l => ps.list forall l.contains)
           
            result <- js.validate[R]
          } yield result
        }

        def writes(r: R): JsValue = {
          f.writes(r).as[JsObject] |
            (_ + ("resourceType" -> JsString(rs.name))) |
            (_ + ("type" -> JsString(bt.value))) |
            (addProfiles(_,ps.list)) |
            (js =>
               r match {
                 case b: Bundle.HasTotal => js + ("total" -> JsNumber(b.total))
                 case _                  => js
               }
            )
        }
      }


//    implicit def resourceLOINCCodedFormat[R <: Resource with HasCode](
    implicit def resourceLOINCCodedFormat[R <: Resource with HasCode, S](
      implicit
      code: Code[R,S],
      f: Format[R],
      rs: Resource.Type[R],
      ps: Meta.Profiles[R],
      bcf: Format[BasicCodeableConcept[S]]
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {

          for {
            rsType <- (js \ "resourceType").validate[String]
                        .filter(JsError(s"Invalid or missing attribute 'resourceType'; expected ${rs.name}"))(_ == rs.name)
            profile <- (js \ "meta" \ "profile").validate[List[URI]]
                         .filter(JsError(s"Invalid or unexpected profiles; expected ${ps.list}"))(l => ps.list forall l.contains)
            codeOk  <- (js \ "code").validate[BasicCodeableConcept[S]]
                         .filter(
                           JsError(s"Invalid or missing CodeableConcept attribute 'code'; expected ${code}")
                         )(
                           _.coding.head.code == code.value.coding.head.code
                         )
            result <- js.validate[R]
          } yield result
        }

        def writes(r: R): JsValue = {
          f.writes(r).as[JsObject] |
            (_ + ("resourceType" -> JsString(rs.name))) |
            (_ + ("code"         -> Json.toJson(code.value))) |
            (addProfiles(_,ps.list))
        }
      }


    implicit def backboneElementLOINCCodedFormat[R <: BackboneElement[_] with HasCode, S](
      implicit
      code: Code[R,S],
      f: Format[R],
      bcf: Format[BasicCodeableConcept[S]]
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {

          (js \ "code").validate[BasicCodeableConcept[S]]
            .filter(
              JsError(s"Invalid or missing CodeableConcept attribute 'code'; expected ${code}")
            )(
              _.coding.head.code == code.value.coding.head.code
            )
            .flatMap(_ => js.validate[R])
        }

        def writes(r: R): JsValue = {
          f.writes(r).as[JsObject] |
            (_ + ("code" -> Json.toJson(code.value)))
        }
      }

/*
    implicit def resourceLOINCCodedFormat[R <: Resource with HasLOINCCode](
      implicit
      code: LOINC.Code[R],
      f: Format[R],
      rs: Resource.Type[R],
      ps: Meta.Profiles[R]
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {

          for {
            rsType <- (js \ "resourceType").validate[String]
                        .filter(JsError(s"Invalid or missing attribute 'resourceType'; expected ${rs.name}"))(_ == rs.name)
            profile <- (js \ "meta" \ "profile").validate[List[URI]]
                         .filter(JsError(s"Invalid or unexpected profiles; expected ${ps.list}"))(l => ps.list forall l.contains)
            codeOk  <- (js \ "code").validate[BasicCodeableConcept[LOINC]]
                         .filter(
                           JsError(s"Invalid or missing CodeableConcept attribute 'code'; expected ${code}")
                         )(
                           _.coding.head.code == code.value.coding.head.code
                         )
            result <- js.validate[R]
          } yield result
        }

        def writes(r: R): JsValue = {
          f.writes(r).as[JsObject] |
            (_ + ("resourceType" -> JsString(rs.name))) |
            (_ + ("code"         -> Json.toJson(code.value))) |
            (addProfiles(_,ps.list))
        }
      }

    implicit def backboneElementLOINCCodedFormat[R <: BackboneElement[_] with HasLOINCCode](
      implicit
      code: LOINC.Code[R],
      f: Format[R],
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {

          (js \ "code").validate[BasicCodeableConcept[LOINC]]
            .filter(
              JsError(s"Invalid or missing CodeableConcept attribute 'code'; expected ${code}")
            )(
              _.coding.head.code == code.value.coding.head.code
            )
            .flatMap(_ => js.validate[R])
        }

        def writes(r: R): JsValue = {
          f.writes(r).as[JsObject] |
            (_ + ("code" -> Json.toJson(code.value)))
        }
      }
*/

    implicit def backboneElementFormat[R <: BackboneElement[_]](
      implicit
      uneq: R <:!< HasCode,
//      uneq: R <:!< HasLOINCCode,
      f: Format[R],
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {
          js.validate[R]
        }
        def writes(r: R): JsValue = {
          f.writes(r)
        }
      }

    implicit def defaultResourceFormat[R <: Resource](
      implicit
      uneq: R <:!< HasCode,
//      uneq: R <:!< HasLOINCCode,
      f: Format[R],
      rs: Resource.Type[R],
      ps: Meta.Profiles[R] 
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {

          for {
            rsType <- (js \ "resourceType").validate[String]
                        .filter(JsError(s"Invalid or missing attribute 'resourceType'; expected ${rs.name}"))(_ == rs.name)
            profile <- (js \ "meta" \ "profile").validate[List[URI]]
                         .filter(JsError(s"Invalid or unexpected profiles; expected ${ps.list}"))(l => ps.list forall l.contains)
            result <- js.validate[R]
          } yield result
        }

        def writes(r: R): JsValue = {

          f.writes(r).as[JsObject] |
            (_ + ("resourceType" -> JsString(rs.name))) |
            (addProfiles(_,ps.list))

        }
      }

  }


  def write[R <: Resource](
    r: R
  )(
    implicit
    rs: Resource.Type[R],
    ps: Meta.Profiles[R] ,
    fhir: FHIRFormat[R]
  ): JsValue =
    fhir.writes(r)


  def read[R <: Resource](
    js: JsValue
  )(
    implicit
    rs: Resource.Type[R],
    fhir: FHIRFormat[R]
  ): JsResult[R] = 
    fhir.reads(js)


  implicit class ResourceOps[R <: Resource](val r: R) extends AnyVal
  {
    def toFHIRJson(
      implicit
      fhir: FHIRFormat[R],
      rs: Resource.Type[R],
      ps: Meta.Profiles[R] 
    ): JsValue = {
     write(r)
    }
  }


  implicit class JsValueOps(val js: JsValue) extends AnyVal
  {
    def asFHIR[R <: Resource](
      implicit
      rs: Resource.Type[R],
      fhir: FHIRFormat[R]
    ): JsResult[R] = {
      read[R](js)
    }
  }

}
