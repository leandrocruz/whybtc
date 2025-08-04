package whybtc


object ui {

  import com.raquo.laminar.api.L.*

  trait InputHandler[T] {
    def parse(str: String): T
    def render(value: T): String
  }

  given InputHandler[Int] = new InputHandler[Int] {
    override def parse(str: String) = str.toInt
    override def render(value: Int) = value.toString
  }

  given InputHandler[Double] = new InputHandler[Double] {
    override def parse(str: String)    = str.toDouble
    override def render(value: Double) = value.toString
  }

  given InputHandler[BigDecimal] = new InputHandler[BigDecimal] {
    override def parse(str: String)        = BigDecimal(str.toDouble)
    override def render(value: BigDecimal) = value.toString
  }

  object InputField {
    def apply[T](name: String, theValue: Var[T])(using handler: InputHandler[T]) = {
      div(
        cls("flex flex-col max-w-64"),
        div(name),
        input(
          cls("px-2 py-1 border rounded"),
          value <-- theValue.signal.map(handler.render),
          onInput.mapToValue.map(handler.parse) --> theValue
//          controlled(
//          )
        )
      )
    }
  }
}