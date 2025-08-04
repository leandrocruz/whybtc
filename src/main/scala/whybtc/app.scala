package whybtc

object login {

  import medulla.UserToken
  import medulla.login.DefaultLoginHelper
  import com.raquo.airstream.core.EventStream

  class WhyBtcLoginHelper extends DefaultLoginHelper {
    override def retrieve = EventStream.fromValue {
      Some {
        new UserToken[Long] {
          override def id    = 1
          override def name  = "Leandro Cruz"
          override def email = "leandro@medulla.com"
        }
      }
    }
  }
}

object WhyBtcPrivateRouter {

  import medulla.router.MedullaRouter
  import com.raquo.waypoint.*
  import io.circe.*
  import io.circe.generic.semiauto.*

  sealed trait Page
  case object BindValuePage           extends Page
  case object HomePage                extends Page
  case object ModalPage               extends Page
  case class UnknownPage(str: String) extends Page

  given Decoder[Page] = deriveDecoder
  given Encoder[Page] = deriveEncoder

  val router = new MedullaRouter[Page](UnknownPage.apply) (
    Route.static(BindValuePage, root / "bind" ),
    Route.static(ModalPage    , root / "modal"),
    Route.static(HomePage     , root / "home" )
  )
}

object render {

  import WhyBtcPrivateRouter.*
  import medulla.ui.layout.SimpleGridLayout
  import medulla.ui.modal.Modal
  import medulla.render.AppRender
  import medulla.UserToken
  import com.raquo.laminar.api.L.*
  import org.scalajs.dom.*

  object HomePageView {
    def apply(): HtmlElement = h1("HOME")
  }

  object BindValuePageView {
    def apply(): HtmlElement = {
      val value = Var("")
      div(
        cls("flex flex-col"),
        div("Value: ", child.text <-- value),
        input(
          cls("p-2 my-4 rounded-md"),
          typ("text"),
          onInput.mapToValue --> value
        ),
      )
    }
  }

  object ModalPageView {
    def apply(): HtmlElement = {

      val opened = Var(false)
      val content = button("Hide", onClick.mapTo(false) --> opened)

      div(
        button(cls("p"), "Show", onClick.mapTo(true)  --> opened),
        child.maybe <-- Modal(content, opened)
      )
    }
  }

  object UnknownPageView {
    def apply(s: String): HtmlElement = div(s"Unknown page for '$s'")
  }

  class WhyBtcAppRender extends AppRender[Long] {

    override def whenLoggedOut = PublicPage()

    override def whenLoggedIn(user: UserToken[Long]) = {

      val main = router.render {
        case BindValuePage    => BindValuePageView()
        case HomePage         => HomePageView()
        case ModalPage        => ModalPageView()
        case UnknownPage(str) => UnknownPageView(str)
      }

      val left = Signal.fromValue {
        div(
          div(a(router.linkTo(HomePage)     , "home")),
          div(a(router.linkTo(BindValuePage), "bind")),
          div(a(router.linkTo(ModalPage)    , "modal"))
        )
      }

      SimpleGridLayout(
        Signal.fromValue(div(cls("m-auto font-bold italic text-xl"), "Medulla")),
        Signal.fromValue(h1(s"${user.email} (${user.id})")),
        left,
        main
      ).render
    }
  }
}


