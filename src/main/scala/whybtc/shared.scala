package whybtc

object pricing {
  import squants.market.*

  import whybtc.extensions.{prettyB, prettyM}
  import java.time.LocalDate
  import java.time.temporal.ChronoUnit

  case class Params(
    today     : LocalDate,
    currency  : Currency,
    price     : Money,
    age       : Int,
    dieAt     : Int,
    years     : Int,
    dieDate   : LocalDate,
    inflation : Double,
    taxes     : Double,
    model     : PricingModel
  )

  case class DataPoint(
    date     : LocalDate,
    age      : Int,
    spending : Money,
    price    : Money,
    units    : BigDecimal,
    btc      : BigDecimal
  ) {
    def pretty = s"Year:${date.getYear}, Age:$age Spending:${spending.prettyM}, Price:${price.prettyM}, BTC:${units.prettyB}, TOTAL:${btc.prettyB}"
  }

  sealed trait PricingModel {
    def priceAt(today: LocalDate, priceToday: Money, date: LocalDate): Money = USD(0)
  }

  case class StockToFlow() extends PricingModel
  case class PowerLaw() extends PricingModel

  case class Fixed(value: Double = .25) extends PricingModel {
    override def priceAt(today: LocalDate, priceToday: Money, date: LocalDate): Money = {
      val years = ChronoUnit.YEARS.between(today, date)
      priceToday * Math.pow(1 + value, years)
    }
  }
}

object scenarios {

  import whybtc.pricing.DataPoint
  import whybtc.extensions.{prettyB, prettyM}
  import com.raquo.laminar.api.L.*
  import squants.market.*
  import whybtc.pricing.Params
  import whybtc.ui.InputField

  def renderTable(points: Seq[DataPoint], btc: BigDecimal, unspent: BigDecimal) = {
    table(
      cls("table-auto w-full"),
      tr(th("Year"), th("Age"), th("Spending"), th("BTC Price"), th("Units of BTC"), th("Wallet")),
      points.map { it =>
        val (c, w) = if (it.btc <= 0) ("text-slate-300", btc.prettyB(4)) else ("", it.btc.prettyB(4))
        tr(
          cls(c),
          td(cls("px-4 py-2 text-center"), it.date.getYear),
          td(cls("px-4 py-2 text-center"), it.age),
          td(cls("px-4 py-2 text-center"), it.spending.prettyM()),
          td(cls("px-4 py-2 text-center"), it.price.prettyM()),
          td(cls("px-4 py-2 text-center"), it.units.prettyB(4)),
          td(cls("px-4 py-2 text-center"), w)
        )
      },
      if (unspent > 0) {
        tr(
          td(cls("text-right"), colSpan(5), "Unspent BTC"),
          td(cls("px-4 py-2 text-center"), unspent.prettyB(4))
        )
      } else {
        commentNode("")
      }
    )
  }

  object EstimateDate {

    def render(btc: BigDecimal, expenses: Money, params: Params): Seq[HtmlElement] = {

      def renderMessage(points: Seq[DataPoint], unspent: BigDecimal) = {
        val maybe     = points.find(_.btc > 0)
        val valuation = params.model.priceAt(params.today, params.price, params.dieDate)
        val remaining = valuation * unspent
        maybe match {
          case None        => div(cls("my-4 p border bg-white rounded"), s"Sorry. You can't retire on ${unspent.prettyB(4)} BTC if you plan to spend ${expenses.prettyM()}/year")
          case Some(point) => div(cls("my-4 p border bg-white rounded"), s"According to your assumptions, you could be able to retire after year ${point.date.getYear}. You will be ${point.age} years old. Assuming you die at ${points.last.date.getYear}, you will hold ${unspent.prettyB(4)} BTC valued at ${remaining.prettyM()}.", if (unspent < 0.000001) "" else " Your family will be proud!")
        }
      }

      val spending = expenses * Math.pow(1 + params.inflation, params.years)

      val points = {
        val years = for year <- params.today.getYear to params.dieDate.getYear yield year
        years.reverse.zipWithIndex.map { (year, idx) =>
          val value = if(idx == 0) spending else spending / Math.pow(1 + params.inflation, idx)
          val date  = params.today.plusYears(year - params.today.getYear)
          val price = params.model.priceAt(params.today, params.price, date)
          val units = value / price
          val age   = date.getYear - params.today.getYear + params.age
          DataPoint(date, age, value, price, units, BigDecimal(0))
        }
      }

      var used    = BigDecimal(0)
      val updated = points.map { point =>
        val value = used + point.units
        if(value < btc) {
          used = value
          point.copy(btc = value)
        }
        else point
      }.reverse

      val unspent = btc - used

      Seq(
        renderMessage(updated, unspent),
        renderTable  (updated, btc, unspent)
      )
    }

    def apply(params: Signal[Params]) = {

      val btc      = Var(1.0)
      val expenses = Var(BigDecimal(100_000))

      div(
        cls("flex flex-col gap-6"),
        div(
          h2(cls("font-bold text-2xl"), "Retirement Date Estimation"),
          p("Given the desired retirement expenses and BTC holdings, estimates the possible retirement date, considering the user assumptions"),
        ),
        InputField("BTC"              , btc),
        InputField("Desired Expenses" , expenses),
        children <-- btc.signal.combineWith(expenses).combineWith(params).map((b, e, p) => render(b ,Money(e, p.currency), p))
      )
    }
  }

  object EstimateSpending {
    def apply(price: Signal[Money]) = {
      val btc      = Var(10.22)
      val retireBy = Var(2035)
      val total    = btc.signal.combineWith(price).map { (b, p) => p * b }
      div(
        "Estimates the spending",
        InputField("BTC", btc),
        InputField("Retire By", retireBy),
        div("Expenses: todo"),
        div("Total: ", child.text <-- total.map(_.toFormattedString))
      )

    }
  }

  object EstimateBtc {
    def apply(params: Signal[Params]) = {

      def render(retireBy: Int, expenses: Money, params: Params): Seq[HtmlElement] = {

        def renderMessage(points: Seq[DataPoint], unspent: BigDecimal) = {
          val maybe = points.find(_.btc > 0)
          maybe match {
            case None        => div(cls("my-4 p border bg-white rounded"), s"Sorry. You can't retire on ${unspent.prettyB(4)} BTC if you plan to spend ${expenses.prettyM()}/year")
            case Some(point) =>
              val years = points.count(_.btc == 0)
              div(cls("my-4 p border bg-white rounded"), s"According to your assumptions, you could be able to retire after year ${point.date.getYear}. You will be ${point.age} years old. Assuming you die at ${points.last.date.getYear}, you would have to acquire ${unspent.prettyB(4)} BTC in the next ${years} years. You better hurry. There is only 21 million")
          }
        }

        val spending = expenses * Math.pow(1 + params.inflation, params.years)
        var totalBtc = BigDecimal(0)
        val points = {
          val years = for year <- params.today.getYear to params.dieDate.getYear yield year
          years.reverse.zipWithIndex.map { (year, idx) =>
            val value = if (idx == 0) spending else spending / Math.pow(1 + params.inflation, idx)
            val date  = params.today.plusYears(year - params.today.getYear)
            val price = params.model.priceAt(params.today, params.price, date)
            val units = value / price
            val age   = date.getYear - params.today.getYear + params.age
            val btc   = if(date.getYear >= retireBy) {
              totalBtc += units
              totalBtc
            } else BigDecimal(0)
            DataPoint(date, age, value, price, units, btc)
          }.reverse
        }

        Seq(
          renderMessage(points, totalBtc),
          renderTable  (points, btc = BigDecimal(0), unspent = BigDecimal(0))
        )
      }

      val retireBy = Var(2037)
      val expenses = Var(BigDecimal(100_000))

      div(
        cls("flex flex-col gap-6"),
        div(
          h2(cls("font-bold text-2xl"), "Retirement BTC Estimation"),
          p("Given the desired retirement date and expenses, estimates the quantity of BTC required, considering the user assumptions"),
        ),
        InputField("Desired Retirement Year", retireBy),
        InputField("Desired Expenses"       , expenses),
        children <-- retireBy.signal.combineWith(expenses).combineWith(params).map((b, e, p) => render(b, Money(e, p.currency), p))
      )
    }
  }
}

object extensions {

  import squants.market.Money
  import scala.math.BigDecimal.RoundingMode

  extension (b: BigDecimal) {
    def prettyB(scale: Int = 2): String = b.setScale(scale, RoundingMode.HALF_UP).toString()
  }

  extension (m: Money) {
    def prettyM(scale: Int = 2): String = {
      if(m.amount > 1_000_000_000) {
        (m.amount / 1_000_000_000).prettyB(scale) + "B"
      } else if (m.amount > 1_000_000) {
        (m.amount / 1_000_000).prettyB(scale) + "M"
      } else if (m.amount > 1_000) {
        (m.amount / 1_000).prettyB(scale) + "K"
      } else {
        m.amount.prettyB(scale)
      }
    }
  }
}
