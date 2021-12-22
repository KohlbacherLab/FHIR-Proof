package org.hl7.fhir.r4


import java.net.URI
import scala.util.Try

import play.api.libs.json._

import shapeless.{HList, ::, HNil, Generic, Lazy, <:!<}

import cats.data.NonEmptyList


object json
{

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


  implicit val hnil: Format[HNil] =
    Format[HNil](
      Reads(_ => JsSuccess(HNil)),
      Writes(_ => JsArray.empty)
    )



  trait KeyFeature[T] extends (JsValue => Boolean)
  {
    def exists: Boolean 
  }

  object KeyFeature
  {

    def apply[T](implicit kf: KeyFeature[T]): KeyFeature[T] = kf

    implicit def hasCodeKeyFeature[T <: HasStaticCode, S: CodingSystem](
      implicit code: Code[T,S]
    ): KeyFeature[T] =
      new KeyFeature[T]{
        override val exists: Boolean = true

        override def apply(js: JsValue): Boolean = {
          (js \ "code").validate[CodeableConceptStatic[S]]
            .exists(_.coding.head.code == code.value.coding.head.code)
        }
      }

    implicit def resourceKeyFeature[T <: Resource](
      implicit profiles: Meta.Profiles[T]
    ): KeyFeature[T] =
      new KeyFeature[T]{
        override val exists: Boolean = true

        override def apply(js: JsValue): Boolean = {
          (js \ "meta" \ "profile").validate[List[URI]]
             .exists(uris => profiles.list forall (uris.contains))
        }
      }

    implicit def extensionKeyFeature[T <: Extension](
      implicit url: Extension.Url[T]
    ): KeyFeature[T] =
      new KeyFeature[T]{
        override val exists: Boolean = true

        override def apply(js: JsValue): Boolean = {
          (js \ "url").validate[URI]
            .exists(_ == url.value)
        }
      }

    implicit def bundleEntryKeyFeature[
      R <: Resource,
    ](
     implicit
     rkf: KeyFeature[R]
    ): KeyFeature[Bundle.EntryOf[R]] =
      new KeyFeature[Bundle.EntryOf[R]]{
        override val exists: Boolean = true

        override def apply(js: JsValue): Boolean =
          rkf.apply((js \ "resource").get)
      }
/*

    implicit def bundleEntryKeyFeature[
      R <: Resource,
      T <: Bundle.EntryElement with Bundle.Entry.resource[R]
    ](
     implicit
     rkf: KeyFeature[R]
    ): KeyFeature[T] =
      new json.KeyFeature[T]{
        override val exists: Boolean = true

        override def apply(js: JsValue): Boolean =
          rkf.apply((js \ "resource").get)

   override def toString: String = rkf.toString
      }
*/

    implicit def anyOtherKeyFeature[T]: KeyFeature[T] =
      new KeyFeature[T]{
        override val exists: Boolean = false

        override def apply(js: JsValue): Boolean = true
      }


  }


  private def combine[A,B,C](
    jsA: JsResult[A],
    jsB: JsResult[B]
  )(
    f: (A,B) => C
  ): JsResult[C] = {
    (jsA,jsB) match {
      case (JsSuccess(a,_),JsSuccess(b,_)) => JsSuccess(f(a,b))
      case (a: JsError,b: JsError)         => a ++ b
      case (a: JsError,_)                  => a
      case (_,b: JsError)                  => b
    }
/*
    jsA.fold(
      errs =>
        jsB.fold(
          es => new JsError(errs ++ es),
          b  => new JsError(errs)
        ),
      a =>
        jsB.fold(
          errs => new JsError(errs),
          b  =>   new JsSuccess(f(a,b))
        )
    )
*/
  }

  import scala.collection.Factory

  private def sequence[H,C[X] <: Iterable[X]](rs: C[JsResult[H]])(
    implicit fac: Factory[H,C[H]]
  ): JsResult[C[H]] = {

    import scala.collection.mutable.Builder

    rs.foldLeft(
      JsSuccess(fac.newBuilder).asInstanceOf[JsResult[Builder[H,C[H]]]]
    ){ 
      case (acc,r) => combine(acc,r)(_ += _)
    }
    .map(_.result)

  }


/*
  trait ProductHelperOps
  {

    import FHIRJson._

    implicit def singleHListHead[H, T <: HList](
      implicit
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   = (
                      if (keyFeature.exists)
                        arr.value.find(keyFeature)
                           .map(fh.value.reads)
                      
                      else
                        arr.value.map(fh.value.reads)
                           .find(_.isSuccess)
                     )
                     .getOrElse(JsError(s"No valid Entry found for required element"))
              t   = ft.reads(js)
              ht <- combine(h,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case h :: t =>
            Json.arr(fh.value.writes(h)) ++ ft.writes(t).as[JsArray]
        }
      )


    implicit def optionHListHead[H, T <: HList](
      implicit
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
    ): Format[Option[H] :: T] =
      Format[Option[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   =  if (keyFeature.exists)
                       arr.value
                         .find(keyFeature)
                         .map(fh.value.reads)
                         .map(_.map(Some(_)))
                         .getOrElse(JsSuccess(None))
                     else
                       JsSuccess(
                         arr.value.map(fh.value.reads)
                            .find(_.isSuccess)
                            .map(_.get)
                       )
              t   = ft.reads(js)
              ht <- combine(h,t)(_ :: _)
            } yield ht
        ), 
        Writes {
          case hOpt :: t =>
            hOpt.map(fh.value.writes).map(Json.arr(_)).getOrElse(JsArray.empty) ++ ft.writes(t).as[JsArray]
        }
      )

    import scala.collection.{BuildFrom,Factory}
    
    implicit def iterableHListHead[H, C[X] <: Iterable[X], T <: HList](
      implicit
      keyFeature: KeyFeature[H],
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
              hs  = (
                      if (keyFeature.exists)
                        sequence(
                          arr.value
                            .filter(keyFeature)
                            .map(fh.value.reads) 
                        )
                      else
                        JsSuccess(
                          arr.value.map(fh.value.reads)
                             .filter(_.isSuccess)
                             .map(_.get)
                        )
                     )
                     .map(_.to(fac))

              t   = ft.reads(js)
              ht <- combine(hs,t)(_ :: _)
            } yield ht
          
        ), 
        Writes {
          case hs :: t =>
            new JsArray(hs.map(fh.value.writes).toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def nonEmptyListHListHead[H, T <: HList](
      implicit
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[NonEmptyList[H] :: T] =
      Format[NonEmptyList[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              hs  =  (
                      if (keyFeature.exists)
                        sequence(
                          arr.value
                            .filter(keyFeature)
                            .map(fh.value.reads) 
                        )
                      else
                        JsSuccess(
                          arr.value.map(fh.value.reads)
                             .filter(_.isSuccess)
                             .map(_.get)
                        )
                     )
                     .filterNot(JsError("Expected non-empty List, but found empty list (or list with invalid entries)"))(_.isEmpty)
                     .map(_.toList)
                     .map(NonEmptyList.fromListUnsafe)

              t   = ft.reads(js)

              ht  <- combine(hs,t)(_ :: _)
            } yield ht
        ), 
        Writes {
          case hs :: t =>
            new JsArray(hs.map(fh.value.writes).toList.toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

  }
*/

  object backboneElements// extends ProductHelperOps
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
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   = (
                      if (keyFeature.exists)
                        arr.value.find(keyFeature)
                           .map(fh.value.reads)
                      
                      else
                        arr.value.map(fh.value.reads)
                           .find(_.isSuccess)
                     )
                     .getOrElse(JsError(s"No valid Entry found for required element"))
              t   = ft.reads(js)
              ht <- combine(h,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case h :: t =>
            Json.arr(fh.value.writes(h)) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def optionBBEHead[H, T <: HList](
      implicit
      valid: IsValidBBEProduct[Option[H] :: T],
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T],
    ): Format[Option[H] :: T] =
      Format[Option[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              h   =  if (keyFeature.exists)
                       arr.value
                         .find(keyFeature)
                         .map(fh.value.reads)
                         .map(_.map(Some(_)))
                         .getOrElse(JsSuccess(None))
                     else
                       JsSuccess(
                         arr.value.map(fh.value.reads)
                            .find(_.isSuccess)
                            .map(_.get)
                       )
              t   = ft.reads(js)
              ht <- combine(h,t)(_ :: _)
            } yield ht
        ), 
        Writes {
          case hOpt :: t =>
            hOpt.map(fh.value.writes).map(Json.arr(_)).getOrElse(JsArray.empty) ++ ft.writes(t).as[JsArray]
        }
      )

/*
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
                      arr.value.map(fh.value.reads)
                        .filter(_.isSuccess)
                        .map(_.get)
                        .toArray
                    )
              t   <- ft.reads(js)
            } yield hs :: t
          
        ), 
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.writes).toVector) ++ ft.writes(t).as[JsArray]
        }
      )
*/

    import scala.collection.{BuildFrom,Factory}
    
    implicit def bbeIterableHead[H, C[X] <: Iterable[X], T <: HList](
      implicit
      valid: IsValidBBEProduct[C[H] :: T],
      keyFeature: KeyFeature[H],
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
              hs  = (
                      if (keyFeature.exists)
                        sequence(
                          arr.value
                            .filter(keyFeature)
                            .map(fh.value.reads) 
                        )
                      else
                        JsSuccess(
                          arr.value.map(fh.value.reads)
                             .filter(_.isSuccess)
                             .map(_.get)
                        )
                     )
                     .map(_.to(fac))

              t   = ft.reads(js)
              ht <- combine(hs,t)(_ :: _)
            } yield ht
          
        ), 
        Writes {
          case hs :: t =>
            new JsArray(hs.map(fh.value.writes).toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def nonEmptyListHListHead[H, T <: HList](
      implicit
      valid: IsValidBBEProduct[NonEmptyList[H] :: T],
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[NonEmptyList[H] :: T] =
      Format[NonEmptyList[H] :: T](
        Reads(
          js => 
            for {
              arr <- js.validate[JsArray]
              hs  =  (
                      if (keyFeature.exists)
                        sequence(
                          arr.value
                            .filter(keyFeature)
                            .map(fh.value.reads) 
                        )
                      else
                        JsSuccess(
                          arr.value.map(fh.value.reads)
                             .filter(_.isSuccess)
                             .map(_.get)
                        )
                     )
                     .filterNot(JsError("Expected non-empty List, but found empty list (or list with invalid entries)"))(_.isEmpty)
                     .map(_.toList)
                     .map(NonEmptyList.fromListUnsafe)

              t   = ft.reads(js)

              ht  <- combine(hs,t)(_ :: _)
            } yield ht
        ), 
        Writes {
          case hs :: t =>
            new JsArray(hs.map(fh.value.writes).toList.toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

  }


  object extensions// extends ProductHelperOps
  {

    implicit def formatExtensionSet[Exts <: ExtensionSet, R](
      implicit 
      gen: Generic.Aux[Exts,R],
      ext: Extension.IsValidExtension[R],
      format: Format[R]
    ): Format[Exts] =
      Format[Exts](
        Reads(js => format.reads(js).map(gen.from)),
        Writes(crs => format.writes(gen.to(crs)))
      )


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
      keyFeature: KeyFeature[H]
//      url: Extension.Url[H]
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray] 
              h   = arr.value 
                      .find(keyFeature)
//                      .find(v => (v \ "url").validate[URI].contains(url.value))
                      .map(fh.value.reads)
                      .getOrElse(JsError(s"Missing Extension in ${js}"))
              t   = ft.reads(arr)
              ht <- combine(h,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case h :: t => Json.arr(fh.value.writes(h)) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def optionExtensionHead[H, T <: HList](
      implicit
      exts: Extension.IsValidExtension[H :: T],
      fh: Lazy[Format[H]],
      ft: Format[T],
//      url: Extension.Url[H]
      keyFeature: KeyFeature[H]
    ): Format[Option[H] :: T] =
      Format[Option[H] :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray] 
              h   =  arr.value 
//                        .find(v => (v \ "url").validate[URI].contains(url.value))
                        .find(keyFeature)
                        .map(fh.value.reads)
                        .map(_.map(Some(_)))
                        .getOrElse(JsSuccess(None))
              t   = ft.reads(arr)
              ht  <- combine(h,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case hOpt :: t =>
            hOpt.map(fh.value.writes).map(Json.arr(_)).getOrElse(JsArray.empty) ++ ft.writes(t).as[JsArray]
        }
      )
    
    
    import scala.collection.{BuildFrom, Factory}
    
    implicit def formatExtensionIterableHead[H, C[X] <: Iterable[X], T <: HList](
      implicit
      exts: Extension.IsValidExtension[C[H] :: T],
      fh: Lazy[Format[H]],
      ft: Format[T],
//      url: Extension.Url[H],
      keyFeature: KeyFeature[H],
      fac: Factory[H,C[H]],
      bf: BuildFrom[C[H],H,C[H]]
    ): Format[C[H] :: T] =
      Format[C[H] :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray] 
              hs  =  sequence(
                       arr.value
//                          .filter(v => (v \ "url").validate[URI].contains(url.value))
                          .filter(keyFeature)
                          .map(fh.value.reads) 
                     )
                     .map(_.to(fac)) 
              t   = ft.reads(arr)
              ht <- combine(hs,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.writes).toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )
    
  }


  object contained// extends ProductHelperOps
  {

    import DomainResource._

    def format[R](implicit f: Format[R]) = f


    implicit def formatContainedResourceSet[CRs <: ContainedResources, R](
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
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[H :: T] =
      Format[H :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray]
              h   =  arr.value
                       .find(keyFeature)
                       .map(fh.value.reads)
                       .getOrElse(JsError(s"Invalid or missing contained resource"))
              t   =  ft.reads(arr)
              ht  <- combine(h,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case h :: t => Json.arr(fh.value.writes(h)) ++ ft.writes(t).as[JsArray]
        }
      )

    implicit def formatContainedResourceOptionalHead[H, T <: HList](
      implicit 
      cr: ForAll[Option[H] :: T, IsContainedResource],
      keyFeature: KeyFeature[H],
      fh: Lazy[FHIRFormat[H]],
      ft: Format[T]
    ): Format[Option[H] :: T] =
      Format[Option[H] :: T](
        Reads(
          js =>
            for {
              arr <- js.validate[JsArray]
              h   =  arr.value
                        .find(keyFeature)
                        .map(fh.value.reads)
                        .map(_.map(Some(_)))
                        .getOrElse(JsSuccess(None))
              t   =  ft.reads(arr)
              ht  <- combine(h,t)(_ :: _)
            } yield ht
                     
        ),
        Writes {
          case hOpt :: t =>
            hOpt.map(fh.value.writes).map(Json.arr(_)).getOrElse(JsArray.empty) ++ ft.writes(t).as[JsArray]
        }
      )

    import scala.collection.{BuildFrom, Factory}

    implicit def formatContainedResourceIterableHead[H, C[X] <: Iterable[X], T <: HList](
      implicit 
      cr: ForAll[C[H] :: T, IsContainedResource],
      keyFeature: KeyFeature[H],
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
              hs  =  sequence(
                       arr.value
                         .filter(keyFeature)
                         .map(fh.value.reads) 
                     )
                     .map(_.to(fac))
              t  =   ft.reads(arr)
              ht <- combine(hs,t)(_ :: _)
            } yield ht
        ),
        Writes {
          case hs :: t => new JsArray(hs.map(fh.value.writes).toIndexedSeq) ++ ft.writes(t).as[JsArray]
        }
      )

  }


}
