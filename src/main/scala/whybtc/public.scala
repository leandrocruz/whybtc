package whybtc

object PublicPage {

  import com.raquo.laminar.api.L.*
  import squants.market.*
  import whybtc.pricing.*
  import whybtc.scenarios.{EstimateBtc, EstimateDate}
  import whybtc.ui.InputField

  import java.time.LocalDate

  def apply(): HtmlElement = {

    val currency        = Var(USD)
    val priceAmount     = Var(BigDecimal(120000))
    val price           = priceAmount.signal.combineWith(currency.signal).map { (a, b) => Money(a, b) }
    val age             = Var(49)
    val dieAt           = Var(99)
    val inflation       = Var(.07)
    val taxes           = Var(.225)
    val growth          = Var(.25)
    val model           = growth.signal.map(Fixed(_))
    //val total         = btc.signal.combineWith(price).map { (b, p) => p * b }

    val params = currency.signal.combineWith(price).combineWith(age.signal).combineWith(dieAt.signal).combineWith(inflation.signal).combineWith(taxes.signal).combineWith(model).map {  (c, p, a, d, i, t, m) =>
      val today   = LocalDate.now()
      val years   = d - a
      val dieDate = today.plusYears(years)
      Params(today, c, p, a, d, years, dieDate, i, t, m )
    }

    div(
      cls("p bg-slate-100"),
      h1(cls("font-bold text-2xl"), "Why Bitcoin"),
      p("Bitcoin retirement calculator"),
      div(
        cls("flex gap-6 my-8"),
        InputField("Age"             , age),
        InputField("Meet with Jesus" , dieAt),
        InputField("BTC Price"       , priceAmount),
        InputField("Inflation"       , inflation),
        InputField("Taxes"           , taxes),
        InputField("Growth"          , growth),
        //div("Total: ", child.text <-- total.map(_.toFormattedString)),
      ),
      div(
        cls("flex flex-col gap-20"),
        EstimateBtc(params),
        EstimateDate(params),
//        EstimateSpending(price),
      )
    )
  }
}