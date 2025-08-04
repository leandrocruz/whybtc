package whybtc

@main
def main(): Unit = {

  import medulla.{DefaultMedullaApp, config}
  import medulla.MedullaApp
  import medulla.config.MedullaConfig
  import wvlet.airframe.*
  import medulla.render.AppRender
  import medulla.login.LoginHelper
  import whybtc.login.WhyBtcLoginHelper
  import whybtc.render.WhyBtcAppRender
  import com.raquo.laminar.api.L.renderOnDomContentLoaded
  import org.scalajs.dom.document

  type UID = Long

  newDesign
    .bind [MedullaConfig   ].toInstance(config.localDev)
    .bind [LoginHelper[UID]].to[WhyBtcLoginHelper]
    .bind [AppRender  [UID]].to[WhyBtcAppRender]
    .bind [MedullaApp      ].to[DefaultMedullaApp[UID]]
    .build[MedullaApp] { app =>
      lazy val container = document.getElementById("app") // must be lazy
      renderOnDomContentLoaded(container, app.rootElement)
    }
}