package org.hl7.fhir.r4


/*
final case class SNOMEDCT
(
  code: String,
  display: Option[String] = None,
  version: Option[String] = None
)
extends Coding
   with Coding.code[Required]
   with Coding.display[Option]
   with Coding.version[Option]
*/

final case class SNOMEDCT(
  code: String,
  display: Option[String]
) extends Coded

object SNOMEDCT
{
  implicit val system = Coding.System[SNOMEDCT]("http://snomed.info/sct")
}
  




/*
object SNOMEDCT
{

  import java.net.URI

  import scala.language.implicitConversions

  implicit val system = Coding.System[SNOMEDCT]("http://snomed.info/sct")

  implicit def toCoding(snomed: SNOMEDCT): Coding[SNOMEDCT] = 
    Coding(snomed.code,snomed.display,Some(Coding.System[SNOMEDCT].uri),None)


  implicit val formatSNOMEDCT =
    play.api.libs.json.Json.format[SNOMEDCT]
}
*/
