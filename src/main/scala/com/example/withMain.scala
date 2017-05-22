package com.example


//Ensayo del nombre de las promociones
import cats.data.{NonEmptyList, Validated}

trait ServiceError{
  val code: String
  val message: String
}

case class PromotionDTO(id: Int, name: String)

case class Promotion(id: Int, name: String)


object Promotion{

  import cats.data.Validated.{ invalidNel, valid }
  import cats.implicits._

  val languageCodesSupported: List[String] = List("EN")
  val countryCodesSupported: List[String] = List("US")

  private def validateLanguageCode(language:String): Boolean = {
    languageCodesSupported.contains(language.toUpperCase())
  }
  private def validateCountryCode(country:String): Boolean = {
    countryCodesSupported.contains(country.toUpperCase())
  }

  private def validateSyntaxCodes(codes: String): Option[(String,String)] = {
    val li = codes.split("-").toList
    val res = li match {
      case languageCode :: countryCode :: Nil => Some((languageCode, countryCode))
      case _ => None
    }
    res
  }
  private def validateSyntaxName(name: String): Option[((String, String),List[String])] = {
    val li = name.split(" ").toList
    val res = li match {
      case cod :: name if validateSyntaxCodes(cod).isDefined => Some((validateSyntaxCodes(cod).get,name))
      case _ => None
    }
    res
  }

  private[this] def validateId(id: Int) : Validated[NonEmptyList[ServiceError],Int] = {
    if(id < 1)
      invalidNel(PromotionIdIsLessThanZero())
    else
      valid(id)
  }

  private[this] def validateName(name: String): Validated[NonEmptyList[ServiceError],String] = {
    val veri = validateSyntaxName(name)
    veri match {
      case Some(_)=> {
        val promoName = veri.get._2
        println(promoName)
        val (lc,cc) = veri.get._1
        if(promoName.size < 1)
          invalidNel(PromotionNameNameIsEmpty())
        else if(!validateLanguageCode(lc))
          invalidNel(PromotionNameLanguageCodeNotSupported())
        else if(!validateCountryCode(cc))
          invalidNel(PromotionNameCountryCodeNotSupported())
        else
          valid(name)
      }
      case None => invalidNel(PromotionNameIsEmpty())
    }
  }

  def validate(id:Int, name: String):Validated[NonEmptyList[ServiceError],Promotion] = {
    (validateId(id) |@| validateName(name)) map {
      (id, name) => Promotion(id,name)
    }
  }
}

case class PromotionIdIsLessThanZero(code: String = "001", message: String = "Promotion id is less than Zero") extends ServiceError

case class PromotionNameIsEmpty(code: String = "002", message: String = "Promotion Name is Empty") extends ServiceError

case class PromotionNameLanguageCodeNotSupported(code:String = "003", message:String ="Language Code Is Not Supported") extends ServiceError

case class PromotionNameCountryCodeNotSupported(code:String = "004", message:String ="Country Code Is Not Supported") extends ServiceError

case class PromotionCodesAreMissing(code:String = "005", message:String ="Codes of the name of promotion are missing") extends ServiceError

case class PromotionNameSyntaxInvalid(code:String = "006", message:String ="Name of promotion is incorrect should be <<iso language code>>-<<iso country code>> <<name-of-promotion>>") extends ServiceError

case class PromotionNameNameIsEmpty (code:String = "007", message:String ="Promotion Name After codes is empty") extends ServiceError


case class CreatePromotion(p: Promotion){
  def createPromotion(p: Promotion): String = "Promotion Created"
  def validar = Promotion.validate(p.id,p.name)
    .fold(
      nel => nel.toList,
      createPromotion
    )
}

object withMain extends App {
  val promotion = Promotion(2, "")
  val cp = CreatePromotion(promotion).validar
}
