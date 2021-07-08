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

    def apply[T](r: Reads[T], w: Writes[T]): FHIRFormat[T] =
      new FHIRFormat[T]{
        def reads(js: JsValue): JsResult[T] = r.reads(js)
        def writes(t: T): JsValue = w.writes(t)
      }

    def apply[T](format: Format[T]): FHIRFormat[T] =
      FHIRFormat(format,format)


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


    import play.api.libs.functional.syntax._


    private def readResourceType[R <: Resource](implicit rs: Resource.Type[R]) =
      (JsPath \ "resourceType").read[String]
        .filter(JsonValidationError(s"Invalid or missing attribute 'resourceType'; expected ${rs.name}"))(_ == rs.name)

    private def readProfiles[R <: Resource](implicit profiles: Meta.Profiles[R]) =
      (JsPath \ "meta" \ "profile").read[List[URI]]
        .filter(JsonValidationError(s"Invalid or unexpected profiles; expected ${profiles.list}"))(l => profiles.list.forall(l.contains))


    implicit def bundleFormat[R <: Bundle](
      implicit
      format: Format[R],
      rs: Resource.Type[R],
      bt: Bundle.Type[R],
      ps: Meta.Profiles[R] 
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = 
        (
          readResourceType[R] and
           
          (JsPath \ "type").read[String]
            .filter(JsonValidationError("Invalid or missing Bundle 'type'"))(_ == bt.value) and
           
          readProfiles[R] and

          format
        )(
          (_,_,_,t) => t
        )
        .reads(js)

        def writes(r: R): JsValue = {
          format.writes(r).as[JsObject] |
            (_ + ("resourceType" -> JsString(rs.name))) |
            (_ + ("type"         -> JsString(bt.value))) |
            (addProfiles(_,ps.list)) |
            (js => {
               import scala.language.reflectiveCalls

               r match {
                 case b: Bundle.HasTotal => js + ("total" -> JsNumber(b.total))
                 case _                  => js
               }
            })
        }
      }
      

/*
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
*/

    private def readCode[R <: HasStaticCode,S](
      implicit
      code: Code[R,S],
      bcf: Format[CodeableConceptStatic[S]]
//      bcf: Format[BasicCodeableConcept[S]]
    ) =
//      (JsPath \ "code").read[BasicCodeableConcept[S]]
      (JsPath \ "code").read[CodeableConceptStatic[S]]
        .filter(
          JsonValidationError(s"Invalid or missing CodeableConcept attribute 'code'; expected ${code}")
        )(
          _.coding.head.code == code.value.coding.head.code
        )

    implicit def resourceLOINCCodedFormat[R <: Resource with HasStaticCode, S](
      implicit
      code: Code[R,S],
      format: Format[R],
      rs: Resource.Type[R],
      ps: Meta.Profiles[R],
      bcf: Format[CodeableConceptStatic[S]]
//      bcf: Format[BasicCodeableConcept[S]]
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {
          (
            readResourceType[R] and
            readProfiles[R] and 
            readCode[R,S] and
            format
          )(
            (_,_,_,t) => t
          )
          .reads(js) 
/*
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
*/
        }

        def writes(r: R): JsValue = {
          format.writes(r).as[JsObject] |
            (_ + ("resourceType" -> JsString(rs.name))) |
            (_ + ("code"         -> Json.toJson(code.value))) |
            (addProfiles(_,ps.list))
        }
      }


    implicit def backboneElementLOINCCodedFormat[R <: BackboneElement[_] with HasStaticCode, S](
      implicit
      code: Code[R,S],
      format: Format[R],
      bcf: Format[CodeableConceptStatic[S]]
//      bcf: Format[BasicCodeableConcept[S]]
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {
          (
            readCode[R,S] and
            format
          )(
            (_,t) => t
          )
          .reads(js) 
/*
          (js \ "code").validate[BasicCodeableConcept[S]]
            .filter(
              JsError(s"Invalid or missing CodeableConcept attribute 'code'; expected ${code}")
            )(
              _.coding.head.code == code.value.coding.head.code
            )
            .flatMap(_ => js.validate[R])
*/
        }

        def writes(r: R): JsValue = {
          format.writes(r).as[JsObject] |
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
      uneq: R <:!< HasStaticCode,
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
      uneq: R <:!< HasStaticCode,
      format: Format[R],
      rs: Resource.Type[R],
      ps: Meta.Profiles[R] 
    ): FHIRFormat[R] =
      new FHIRFormat[R]{
        def reads(js: JsValue): JsResult[R] = {
          (
            readResourceType[R] and
            readProfiles[R] and 
            format
          )(
            (_,_,t) => t
          )
          .reads(js) 
/*
          for {
            rsType <- (js \ "resourceType").validate[String]
                        .filter(JsError(s"Invalid or missing attribute 'resourceType'; expected ${rs.name}"))(_ == rs.name)
            profile <- (js \ "meta" \ "profile").validate[List[URI]]
                         .filter(JsError(s"Invalid or unexpected profiles; expected ${ps.list}"))(l => ps.list forall l.contains)
            result <- js.validate[R]
          } yield result
*/
        }

        def writes(r: R): JsValue = {
          format.writes(r).as[JsObject] |
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
