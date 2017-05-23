//Ensayo del nombre de las promociones
import cats.data.{NonEmptyList, Validated}

trait ServiceError{
  val code: String
  val message: String
}

type Validation[A] = Validated[NonEmptyList[ServiceError],A]

case class PromotionDTO(id: Int, name: String)

case class Promotion(id: Int, name: String)


object Promotion{

  import cats.data.Validated.{ invalidNel, valid }
  import cats.implicits._

  val languageCodesSupported: List[String] = List("EN")
  val countryCodesSupported: List[String] = List("US")

  private[this] def validateLanguageCodeIsSupported(language:String): Validation[String] = {
    if(languageCodesSupported.contains(language.toUpperCase()))
      valid(language)
    else
      invalidNel(PromotionNameLanguageCodeNotSupported())
  }
  private[this] def validateCountryCodeIsSupported(country:String): Validation[String] = {
    if(countryCodesSupported.contains(country.toUpperCase()))
      valid(country)
    else
      invalidNel(PromotionNameCountryCodeNotSupported())
  }

  private[this] def validateSyntaxCodes(codes: String): Validation[String] = {
    val li = codes.split("-").toList
    if(li.size < 2)invalidNel(PromotionCodesFormatInvalid())
    else {
      validateLanguageCodeIsSupported(li(1))
      validateCountryCodeIsSupported(li(2))
    }
  }
  private[this] def validateSyntaxFormatName(name: String): Validation[String] = {
    val li = name.split(" ",2).toList
    if(li.size < 2) invalidNel(PromotionNameSyntaxInvalid())
    else {
      validatePromotionName(li(1))
    }
  }

  private[this] def validateId(id: Int) : Validation[Int] = {
    if(id < 1)
      invalidNel(PromotionIdIsLessThanZero())
    else
      valid(id)
  }

  private[this] def validatePromotionName(name: String): Validation[String] = {
    if(name.isEmpty)
      invalidNel(PromotionNameIsEmpty())
    else valid(name)
  }

  def validate(id:Int, name: String):Validation[Promotion] = {
    (validateId(id) |@| validateSyntaxFormatName(name) ) map {
      (id, name) => Promotion(id,name)
    }

  }
}
case class PromotionIdIsLessThanZero(code: String = "001", message: String = "Promotion id is less than Zero") extends ServiceError

case class PromotionNameIsEmpty(code: String = "002", message: String = "Promotion Name is Empty") extends ServiceError

case class PromotionNameLanguageCodeNotSupported(code:String = "003", message:String ="Language Code Is Not Supported") extends ServiceError

case class PromotionNameCountryCodeNotSupported(code:String = "004", message:String ="Country Code Is Not Supported") extends ServiceError

case class PromotionCodesFormatInvalid(code:String = "005", message:String ="Promotion format codes invalid") extends ServiceError

case class PromotionNameSyntaxInvalid(code:String = "006", message:String ="Name of promotion is incorrect should be <<iso language code>>-<<iso country code>> <<name-of-promotion>>") extends ServiceError



val promotion = Promotion(2,"ffff gggg")


case class CreatePromotion(p: Promotion){
  def createPromotion(p: Promotion): String = "Promotion Created"
  def validar = Promotion.validate(p.id,p.name)
    .fold(
      nel => nel.toList,
      createPromotion
    )
}

val cp = CreatePromotion(promotion).validar


