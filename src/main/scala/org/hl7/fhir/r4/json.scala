package org.hl7.fhir.r4


import scala.util.Try

import play.api.libs.json._

import shapeless.{HList, ::, HNil, Generic, Lazy, <:!<}

import cats.data.NonEmptyList


object json
{

  def format[K](m: Map[K,String]): Format[K] = 
    Format(
      Reads(
        _.validate[String]
         .flatMap(
           s => m.find { case (_,v) => v == s }
                 .map(_._1)
                 .map(JsSuccess(_))
                 .getOrElse(JsError(s"Invalid value $s, expected one of ${m.values}"))
         )
      ),
      Writes(k => Json.toJson(m(k)))
    )


  def formatCodedEnum[E <: CodedEnum](
    e: E
  ): Format[E#Value] =
    Format[E#Value](
      Reads( js =>
        for {
          c <- js.validate[String]
          vs = e.values.unsorted.map(_.asInstanceOf[Coded])
          r <- vs.find(_.code == c)
                 .map(JsSuccess(_))
                 .getOrElse (
                   JsError(s"Invalid code value '$c', expected one of ${vs}")
                 )
        } yield r.asInstanceOf[E#Value]         
      ),
      Writes(
        c => Json.toJson(c.asInstanceOf[Coded].code)
      )
    )


  implicit def formatNel[T: Reads: Writes](
    implicit
    reads: Reads[List[T]],
    writes: Writes[List[T]]
  ): Format[NonEmptyList[T]] =
    Format[NonEmptyList[T]](
      reads
        .filterNot(JsonValidationError("Found empty list where non-empty list expected"))(_.isEmpty)
        .map(NonEmptyList.fromListUnsafe),
      writes.contramap(_.toList)
    )


  object backboneElements
  {

    @annotation.implicitNotFound(
    """${E} is not a valid BackboneElement Product Entry type:
     Only BackboneElement[B] sub-classes, Iterable[_] or NonEmptyList of BackboneElement[B] sub-classes are allowed"""
    )
    sealed trait IsValidEntry[E]
    object IsValidEntry
    {
      def apply[E](implicit is: IsValidEntry[E]) = is

      implicit def entry[E <: BackboneElement[_]]: IsValidEntry[E] = 
        new IsValidEntry[E]{}

      implicit def entryOption[E: IsValidEntry]: IsValidEntry[Option[E]] = 
        new IsValidEntry[Option[E]]{}

      implicit def entrySeq[E <: BackboneElement[Many], S[X] <: Iterable[X]]: IsValidEntry[S[E]] = 
        new IsValidEntry[S[E]]{}

      implicit def entryArray[E <: BackboneElement[Many]]: IsValidEntry[Array[E]] = 
        new IsValidEntry[Array[E]]{}

      implicit def entryNel[E <: BackboneElement[Many]]: IsValidEntry[NonEmptyList[E]] = 
        new IsValidEntry[NonEmptyList[E]]{}
    }


    @annotation.implicitNotFound(
    "${BBEs} is not a valid BackboneElement Product. Ensure all Entries have the same BackboneElement base"
    )
    sealed trait IsValidBBEProduct[BBEs]
    object IsValidBBEProduct
    {
      def apply[BBEs](implicit valid: IsValidBBEProduct[BBEs]) = valid

      implicit def genericBBEProduct[BBEs <: Product, R](
        gen: Generic.Aux[BBEs,R],
        valid: IsValidBBEProduct[R]
      ): IsValidBBEProduct[BBEs] = new IsValidBBEProduct[BBEs]{}

      implicit def hlistAllSameBBEBase[H, T <: HList](
        implicit
        valid: ForAll[H :: T, IsValidEntry]
      ): IsValidBBEProduct[H :: T] = new IsValidBBEProduct[H :: T]{}

      implicit def hnilBBEBase: IsValidBBEProduct[HNil] = new IsValidBBEProduct[HNil]{}
    }



    implicit def formatBBEProduct[BBEs <: Product, R](
      implicit 
      gen: Generic.Aux[BBEs,R],
      format: Lazy[Format[R]]
    ): Format[BBEs] = 
      Format[BBEs](
        format.value.map(gen.from),
        format.value.contramap(gen.to)
      )

    import FHIRJson._

    implicit def singleBBEHead[H, T <: HList](
      implicit
      valid: IsValidBBEProduct[H :: T],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   <- arr.value.map(fh.value.read)
                       .find(_.isSuccess)
                       .getOrElse(JsError(s"No valid Entry found in ${arr}"))
              t   <- ft.reads(js)
            } yield h :: t
        ), 
        Writes {
          case h :: t => Json.arr(fh.value.write(h)) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def optionBBEHead[H, T <: HList](
      implicit
      valid: IsValidBBEProduct[Option[H] :: T],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
    ): Format[Option[H] :: T] =
      Format[Option[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   <- JsSuccess(
                       arr.value.map(fh.value.reads)
                         .find(_.isSuccess)
                         .map(_.get)
                     )           
              t   <- ft.reads(js)
            } yield h :: t
        ), 
        Writes {
          case hOpt :: t =>
            hOpt.map(fh.value.write).map(Json.arr(_)).getOrElse(JsArray.empty) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def bbeArrayHead[H, T <: HList](
      implicit
      ct: scala.reflect.ClassTag[H],
      valid: IsValidBBEProduct[Array[H] :: T],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[Array[H] :: T] =
      Format[Array[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              hs  <- JsSuccess(
                      arr.value.map(fh.value.read)
                        .filter(_.isSuccess)
                        .map(_.get)
                        .toArray
                    )
              t   <- ft.reads(js)
            } yield hs :: t
          
        ), 
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.write).toVector) ++ ft.writes(t).as[JsArray]
        }
      )


    import scala.collection.{BuildFrom,Factory}
    
    implicit def bbeIterableHead[H, C[X] <: Iterable[X], T <: HList](
      implicit
      valid: IsValidBBEProduct[C[H] :: T],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
      fac: Factory[H,C[H]],
      bf: BuildFrom[C[H],H,C[H]]
    ): Format[C[H] :: T] =
      Format[C[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              hs  <- JsSuccess(
                      arr.value.map(fh.value.read)
//                        .tapEach(_.fold(println, _ => ()))
                        .filter(_.isSuccess)
                        .map(_.get)
                        .to(fac)
                    )
              t   <- ft.reads(js)
            } yield hs :: t
          
        ), 
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.write).toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def bbeNelHead[H, T <: HList](
      implicit
      valid: IsValidBBEProduct[NonEmptyList[H] :: T],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[NonEmptyList[H] :: T] =
      Format[NonEmptyList[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              hs  <- NonEmptyList.fromList(
                      arr.value.map(fh.value.reads)
                        .filter(_.isSuccess)
                        .map(_.get)
                        .toList
                     )
                     .map(JsSuccess(_))
                     .getOrElse(JsError("Expected non-empty List, but found empty list (or list with invalid entries)"))
              t   <- ft.reads(js)
            } yield hs :: t
        ), 
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.write).toList.toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

/*
    implicit def optionBBEIterableHead[H <: BackboneElement[Many], C[X] <: Iterable[X], T <: HList](
      implicit
      valid: IsValidBBEProduct[Option[C[H]] :: T],
      fh: Lazy[Format[H]],
      ft: Format[T],
      fac: Factory[H,C[H]],
      bf: BuildFrom[C[H],H,C[H]]
    ): Format[Option[C[H]] :: T] =
      Format[Option[C[H]] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   <- JsSuccess(
                       Option(
                         arr.value.map(fh.value.reads)
                           .filter(_.isSuccess)
                           .map(_.get) 
                           .to(fac)
                       )
                       .filter(_.isEmpty)
                     )           
              t   <- ft.reads(js)
            } yield h :: t
        ), 
        Writes {
          case hOpt :: t =>
            hOpt.map(_.map(fh.value.writes)).map(_.toIndexedSeq).map(new JsArray(_)).getOrElse(JsArray.empty) ++ ft.writes(t).as[JsArray]
        }
      )
*/
    implicit val hnil: Format[HNil] =
      Format[HNil](
        Reads(_ => JsSuccess(HNil)),
        Writes(_ => JsArray.empty)
      )

  }


  object extensions
  {

    def format[V: Format, E <: SimpleExtension[V]](
      f: V => E
    )(
      implicit
      url: Extension.Url[E],
      vt: ValueElement[V]
    ): Format[E] = 
      Format[E](
        Reads(
          //TODO: validate Extension.Url
          js => (js \ vt.name).validate[V].map(f)
        ),
        Writes( e =>
          Json.obj(
            "url"   -> url.value,
            vt.name -> Json.toJson(e.value)
          )
        )
      )
    
    
    def format[E](implicit f: Format[E]) = f
    
    implicit def compositeExtension[E <: Product with Extension,R](
      implicit
      gen: Generic.Aux[E,R],
      ext: Extension.IsValidExtension[R],
      f: Lazy[Format[R]],
      url: Extension.Url[E]
    ): Format[E] = 
      Format[E](
        Reads(
          //TODO: validate Extension.Url
          js => (js \ "extension")
                  .validate[JsArray]
                  .flatMap(f.value.reads(_).map(gen.from))
        ),
        Writes( ext =>
          Json.obj(
            "url"       -> url.value,
            "extension" -> f.value.writes(gen.to(ext))
          )
        )
      )
    
    implicit def extensionHList[H, T <: HList](
      implicit
      exts: Extension.IsValidExtension[H :: T],
      fh: Lazy[Format[H]],
      ft: Format[T],
      url: Extension.Url[H]
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray] 
              h   <- arr.value 
                       .find(v => (v \ "url").as[String] == url.value.toString)
                       .map(h => fh.value.reads(h))
                       .getOrElse(JsError(s"Missing Extension '${url.value}' in ${js}"))
              t   <- ft.reads(arr)
            } yield h :: t
            
        ),
        Writes {
          case h :: t => Json.arr(fh.value.writes(h)) ++ Json.toJson(t).as[JsArray]
        }
      )
    
    import scala.collection.{BuildFrom, Factory}
    
    implicit def formatIterableHead[H, C[X] <: Iterable[X], T <: HList](
      implicit
      exts: Extension.IsValidExtension[C[H] :: T],
      fh: Lazy[Format[H]],
      ft: Format[T],
      url: Extension.Url[H],
      fac: Factory[H,C[H]],
      bf: BuildFrom[C[H],H,C[H]]
    ): Format[C[H] :: T] =
      Format[C[H] :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray] 
              h   <- JsSuccess(
                       arr.value 
                         .filter(v => (v \ "url").as[String] == url.value.toString)
                         .map(fh.value.reads)
                         .filter(_.isSuccess)
                         .map(_.get)
                         .to(fac)
                     )
              t   <- ft.reads(arr)
            } yield h :: t
            
        ),
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.writes).toIndexedSeq) ++ Json.toJson(t).as[JsArray]
        }
      )
    
    
    implicit val hnil: Format[HNil] =
      Format[HNil](
        Reads(_ => JsSuccess(HNil)),
        Writes(_ => JsArray.empty)
      )

  }


  object contained 
  {

    import DomainResource._

    implicit def formatContainedResourceSet[CRs <: Product, R](
      implicit 
      gen: Generic.Aux[CRs,R],
      format: Format[R]
    ): Format[CRs] =
      Format[CRs](
        Reads(js => format.reads(js).map(gen.from)),
        Writes(crs => format.writes(gen.to(crs)))
      )

    import FHIRJson._

    implicit def formatContainedResourceHead[H, T <: HList](
      implicit 
      cr: ForAll[H :: T, IsContainedResource],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray]
              h   <- arr.value.map(fh.value.reads)
                      .find(_.isSuccess)
                      .getOrElse(JsError(s"Invalid or missing contained resource"))
              t   <- ft.reads(arr)
            } yield h :: t
        ),
        Writes {
          case h :: t => Json.arr(fh.value.write(h)) ++ ft.writes(t).as[JsArray]
        }
      )

    import scala.collection.{BuildFrom, Factory}

    implicit def formatContainedResourceIterableHead[H, C[X] <: Iterable[X], T <: HList](
      implicit 
      cr: ForAll[C[H] :: T, IsContainedResource],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
      fac: Factory[H,C[H]],
      bf: BuildFrom[C[H],H,C[H]]
    ): Format[C[H] :: T] =
      Format[C[H] :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray]
              hs  <- JsSuccess(
                       arr.value
                         .map(fh.value.reads)
                         .filter(_.isSuccess)
                         .map(_.get)
                         .to(fac)
                     )
              t   <- ft.reads(arr)
            } yield hs :: t
        ),
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.write).toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )


    implicit val hnil: Format[HNil] =
      Format[HNil](
        Reads(_ => JsSuccess(HNil)),
        Writes(_ => JsArray.empty)
      )


  }


}
