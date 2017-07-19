import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}


case class PointType(
                    id:String,
                    status:String,
                    code: String,
                    name:String,
                    createTime:String,
                    updateTime:String
                    )

case class PointAmount(
                      pointType:String,
                      amount:String
                      )

case class Tier(
               id:String,
               number:Int,
               status:String,
               code:String,
               name:String,
               externalName:String,
               top:String,
               default:String,
               pointTypes:List[String],
               entryThreshold: PointAmount,
               exitThreshold:Option[PointAmount],
               createTime:String,
               updateTime:String
               )

case class Program(
                  id:String,
                  status:String,
                  typ: String,
                  name:String,
                  geography:String,
                  currency:String,
                  startTime:String,
                  createTime:String,
                  updateTime:String,
                  pointTypes: List[PointType],
                  tiers: List[Tier]
                  )

class testCirce extends FlatSpec with Matchers{

  "circe" should "parse" in {

    case class Card(
                     svcNumber: String,
                     registrationDate: String,
                     cardClass: String,
                     promoCode: String,
                     status: String,
                     cardId: String,
                     cardCreatedOnDate: String,
                     cardChangedOnDate: String,
                     other1: String,
                     other2: String
                   )

    implicit val encodeCard: Encoder[Card] =
      Encoder.forProduct10(
        "INTEGRATION_ID",
        "MEMBER_WID",
        "PR_CON_ID",
        "X_CARD_NUM",
        "START_DT",
        "X_STATUS_CD",
        "X_CARD_CLASS",
        "X_PROMO_ID",
        "CREATED_ON_DT",
        "CHANGED_ON_DT"
      )(
        c =>
          (
            c.svcNumber,
            c.registrationDate,
            c.cardClass,
            c.promoCode,
            c.status,
            c.cardId,
            c.cardCreatedOnDate,
            c.cardChangedOnDate,
            c.other1,
            c.other2
          )
      )

    val se: Json = Card(
      "Foo",
      "McBar",
      "cardClass",
      "promoCode",
      "status",
      "cardId",
      "cardCreatedOnDate",
      "cardChangedOnDate",
      "other1",
      "other2"
    ).asJson

    se.toString shouldBe "{\n  \"INTEGRATION_ID\" : \"Foo\",\n  \"MEMBER_WID\" : \"McBar\",\n  \"PR_CON_ID\" : \"cardClass\",\n  \"X_CARD_NUM\" : \"promoCode\",\n  \"START_DT\" : \"status\",\n  \"X_STATUS_CD\" : \"cardId\",\n  \"X_CARD_CLASS\" : \"cardCreatedOnDate\",\n  \"X_PROMO_ID\" : \"cardChangedOnDate\",\n  \"CREATED_ON_DT\" : \"other1\",\n  \"CHANGED_ON_DT\" : \"other2\"\n}"

  }

  "ucp-event-publusher" should "map program to point type data" in{

    implicit val encodePointAmount: Encoder[PointAmount] =
      Encoder.forProduct2(
        "POINT_TYPE",
        "AMOUNT"
      )(
        pa => (
          pa.pointType,
          pa.amount
        )
      )

    implicit val encodeTier: Encoder[Tier] ={
      Encoder.forProduct13(
        "INTEGRATION_ID",
        "NUMBER(NO)",
        "ACTIVE_FLG",
        "CODE(NO)",
        "TIER_NAME(NO)",
        "TIER_NAME",
        "TOP(NO)",
        "DEFAULT(NO",
        "POINT_TYPES(NO)",
        "ENTRY_THRE(NO)",
        "EXIT_THRE(NO)",
        "CREATED_ON_DT",
        "UPDATE_ON_DT"
      )(
        t => (
          t.id,
          t.number,
          t.status,
          t.code,
          t.name,
          t.externalName,
          t.top,
          t.default,
          t.pointTypes,
          t.entryThreshold ,
          t.exitThreshold,
          t.createTime,
          t.updateTime

        )
      )
    }

    implicit val encodePointType: Encoder[PointType] =
      Encoder.forProduct6(
        "INTEGRATION_ID",
        "ACTIVE_FLG",
        "CODE(NO)",
        "POINT_TYPE_NAME",
        "CREATED_ON_DT",
        "UPDATED_ON_DT"
      )(
        pt => (
          pt.id,
          pt.status,
          pt.code,
          pt.name,
          pt.createTime,
          pt.updateTime
        )
      )

    implicit val encodeProgram: Encoder[Program] =
      Encoder.forProduct11(
        "INTEGRATION_ID",
        "ACTIVE_FLG",
        "TYPE(NO)",
        "PROGRAM",
        "GEOGRAPHY(NO)",
        "CURRENCY(NO)",
        "START_DT",
        "CREATED_ON_DT",
        "CHANGED_ON_DT",
        "POINT_TYPES",
        "TIERS"
      )(
        p =>
          (
            p.id,
            p.status,
            p.typ,
            p.name,
            p.geography,
            p.currency,
            p.startTime,
            p.createTime,
            p.updateTime,
            p.pointTypes,
            p.tiers
          )
      )

    val program = Program(
      "id",
      "status",
      "type",
      "name",
      "geography",
      "currency",
      "startTime",
      "createTime",
      "updateTime",
      List(
        PointType(
          "id1",
          "statusPointType1",
          "codePointTYpe1",
          "namePointType1",
          "createTimePointType1",
          "updateTimePointType1"
        ),
        PointType(
          "id2",
          "statusPointType2",
          "codePointTYpe2",
          "namePointType2",
          "createTimePointType2",
          "updateTimePointType2"
        ),
        PointType(
          "id3",
          "statusPointType3",
          "codePointTYpe3",
          "namePointType3",
          "createTimePointType3",
          "updateTimePointType3"
        ),
        PointType(
          "id4",
          "statusPointType4",
          "codePointTYpe4",
          "namePointType4",
          "createTimePointType4",
          "updateTimePointType4"
        )
      ),
      List(
        Tier(
          "idTier1",
          1,
          "statusTier1",
          "codeTier1",
          "nameTier1",
          "externalNameTier1",
          "top1",
          "defaultTier1",
          List(
            "id1",
            "id2"
          ),
          PointAmount(
            "pointType1",
            "amount1"
          ),
          None,
          "createTimeTier1",
          "updateTimeTier1"
        ),
        Tier(
          "idTier2",
          2,
          "statusTier2",
          "codeTier2",
          "nameTier2",
          "externalNameTier2",
          "top2",
          "defaultTier2",
          List(
            "id1",
            "id2"
          ),
          PointAmount(
            "pointType2",
            "amount2"
          ),
          None,
          "createTimeTier2",
          "updateTimeTier2"
        )
      )
    )

    val pointTyperOfProgram = program.pointTypes



    println(program.toString)

  }

}
