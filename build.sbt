import org.scalajs.linker.interface.{ESVersion, ModuleSplitStyle}

lazy val medulla = project.in(file("."))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .settings(
    name := "whybtc",
    scalaVersion := "3.3.3",

    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the "livechart" package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _ .withESFeatures(_.withESVersion(ESVersion.ES2018))
        .withModuleKind(ModuleKind.ESModule)
        .withSourceMap(true)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("medulla")))
    },

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies ++= List(
      "medulla"       %%% "medulla" % "0.1.0-SNAPSHOT" changing(),
      "org.typelevel" %%% "squants" % "1.8.3"
    )
  )
