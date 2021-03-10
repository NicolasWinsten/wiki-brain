name := "wiki-brain"

version := "0.1"

scalaVersion := "2.13.5"

enablePlugins(ScalaJSPlugin)
// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.1"

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
