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

  object EstimateDate {
    def apply(params: Signal[Params]) = {

      val btc       = Var(1.0)
      val expenses  = Var(BigDecimal(100000))
      val total     = params.combineWith(btc.signal).map { _.price * _ }
      val valuation = params.map { p => p.model.priceAt(p.today, p.price, p.dieDate) }
      val expensesM = params.combineWith(expenses.signal).map { (p, e) => Money(e, p.currency) }
      val spending  = params.combineWith(expensesM).map { (p, e) => e * Math.pow(1 + p.inflation, p.years) }
      val message   = params.combineWith(valuation).combineWith(spending).map { (p, v, s) =>
        s"""
          | You will die at ${p.dieDate}, ${p.years} years from today.
          | At this date, Bitcoin will be valued at ${v.prettyM} per BTC.
          | Yor annual spending will be ${s.prettyM}
          |""".stripMargin
      }

      val points = params.combineWith(spending).map { (p, s) =>
        val years = for year <- p.today.getYear to p.dieDate.getYear yield year
        years.reverse.zipWithIndex.map { (year, idx) =>
          val value = if(idx == 0) s else s / Math.pow(1 + p.inflation, idx)
          val date  = p.today.plusYears(year - p.today.getYear)
          val price = p.model.priceAt(p.today, p.price, date)
          val units = value / price
          val age   = date.getYear - p.today.getYear + p.age
          DataPoint(date, age, value, price, units, BigDecimal(0))
        }
      }

      val updated = points.combineWith(btc).map { (seq, b) =>
        var used   = BigDecimal(0)
        val result = seq.map { point =>
          val value = used + point.units
          if(value < b) {
            used = value
            point.copy(btc = value)
          }
          else point
        }

        (result, b - used /* unspent */)
      }

      val retirement = params.combineWith(updated).map { (p, points, unspent) =>
        (
          unspent,
          points.findLast(_.btc > 0),
          p.dieDate,
          p.model.priceAt(p.today, p.price, p.dieDate) * unspent
        )
      }

      div(
        cls("flex flex-col gap-6"),
        h2(cls("font-bold text-2xl"), "Retirement Date Estimation"),
        InputField("BTC"      , btc),
        InputField("Expenses" , expenses),
        //div("Money: ", child.text <-- total.map(_.pretty)),
        //child.text <-- message,
        child <-- retirement.combineWith(expensesM).map {
          case (unspent, None      , _, _, spending) => div(cls("my-4 p border bg-white rounded"), s"Sorry. You can't retire on ${unspent.prettyB(4)} BTC if you plan to spend ${spending.prettyM()}/y")
          case (unspent, Some(year), last, price, _) => div(cls("my-4 p border bg-white rounded"), s"You can retire after year ${year.date.getYear}. You will be ${year.age} years old. When you die at ${last.getYear}, you will hold ${unspent.prettyB(4)} BTC valued at ${price.prettyM()}.", if(unspent < 0.000001) "" else " Your family will be proud!" )
        },
        table(
          cls("table-auto w-full"),
          tr(th("Year"), th("Age"), th("Spending"), th("BTC Price"), th("Units of BTC"), th("Wallet")),
          children <-- updated.combineWith(btc).map { (items, unspent, wallet) =>
            items.reverse.map { it =>
              val (c, w) = if(it.btc <=0) ("bg-red-300", wallet.prettyB(4)) else ("", it.btc.prettyB(4))

              tr(
                cls(c),
                td(cls("px-4 py-2 text-center"), it.date.getYear),
                td(cls("px-4 py-2 text-center"), it.age),
                td(cls("px-4 py-2 text-center"), it.spending.prettyM()),
                td(cls("px-4 py-2 text-center"), it.price.prettyM()),
                td(cls("px-4 py-2 text-center"), it.units.prettyB(4)),
                td(cls("px-4 py-2 text-center"), w)
              )
            }
          }
        )
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
    def apply(price: Signal[Money]) = {

      val retireBy = Var(2035)
      val expenses = Var(BigDecimal(300000))
      div(
        "Estimates the BTC",
        InputField("Retire By", retireBy),
        InputField("Expenses", expenses),
        div("Btc: todo")
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
