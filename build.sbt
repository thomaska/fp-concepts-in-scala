enablePlugins(TutPlugin, GhpagesPlugin)

organization := "tech.monoid"
scalaVersion := "2.12.11"

val tutDirName = settingKey[String]("tut output directory")
tutDirName := "./"

addMappingsToSiteDir(tut, tutDirName)
includeFilter in SitePlugin.autoImport.makeSite :=
    "*.yml" | "*.md" | "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.eot" | "*.svg" | "*.ttf" |
    "*.woff" | "*.woff2" | "*.otf"

git.remoteRepo := "git@github.com:thomaska/fp-concepts-in-scala.git"


(scalacOptions in Tut) += "-Xfatal-warnings"
