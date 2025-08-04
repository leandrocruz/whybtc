package whybtc

object PublicPage {

  import com.raquo.laminar.api.L.*
  import squants.market.*
  import whybtc.pricing.*
  import whybtc.scenarios.{EstimateBtc, EstimateDate}
  import whybtc.ui.InputField

  import java.time.LocalDate

  def apply(): HtmlElement = {

    def buildModel(typ: String, growth: Double) = {
      typ match
        case "Power Law" => PowerLaw()
        case "Custom"    => Fixed(growth)
    }

    def dateToReach(target: Double)(model: PricingModel, params: Params) = model.dateToReach(params.today, params.price, target).toString

    val currency        = Var(USD)
    val priceAmount     = Var(BigDecimal(120000))
    val price           = priceAmount.signal.combineWith(currency.signal).map { (a, b) => Money(a, b) }
    val age             = Var(49)
    val dieAt           = Var(99)
    val inflation       = Var(.07)
    val taxes           = Var(.225)
    val modelType       = Var("Power Law")
    val growth          = Var(.25)
    val model           = modelType.signal.combineWith(growth.signal).map(buildModel)

    val params = currency.signal.combineWith(price).combineWith(age.signal).combineWith(dieAt.signal).combineWith(inflation.signal).combineWith(taxes.signal).combineWith(model).map {  (c, p, a, d, i, t, m) =>
      val today   = LocalDate.now()
      val years   = d - a
      val dieDate = today.plusYears(years)
      Params(today, c, p, a, d, years, dieDate, i, t, m )
    }

    div(
      cls("p"),
      h1(cls("font-bold text-2xl"), "Why Bitcoin"),
      p("Bitcoin retirement calculator"),
      div(
        cls("flex flex-row justify-start items-start flex-wrap gap-6 my-8"),
        InputField("Age"             , age),
        InputField("Meet with Jesus" , dieAt),
        InputField("BTC Price"       , priceAmount),
        InputField("Inflation"       , inflation),
        InputField("Taxes"           , taxes),
        div(
          cls("flex flex-col max-w-64"),
          div("Pricing Model"),
          select(
            cls("px-2 py-1 border rounded h-[34px]"),
            option("Power Law"),
            option("Custom"),
            onInput.mapToValue --> modelType
          )
        ),
        InputField("Growth", growth, modelType.signal.map(_ != "Custom")),
        //div("Total: ", child.text <-- total.map(_.toFormattedString)),
      ),
      div(
        cls("flex flex-col gap-20"),
        div(
          cls("flex flex-col gap-4"),
          div(
            h2(cls("font-bold text-2xl"), "Milestones"),
            p("According to the pricing you've chosen, when BTC will hit 1M and 10M"),
          ),
          div(
            div("1M - " , child.text  <-- model.combineWith(params).map(dateToReach(1_000_000))),
            div("10M - ", child.text <-- model.combineWith(params).map(dateToReach(10_000_000)))
          )
        ),
        EstimateBtc(params),
        EstimateDate(params),
//        EstimateSpending(price),
      )
    )
  }
}