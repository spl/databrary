package controllers

import scala.concurrent.Future
import play.api.Play
import play.api.Play.current
import play.api.data.Forms
import play.api.http.HeaderNames
import play.api.i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import play.api.mvc._
import org.apache.commons.{io => IO}
import macros._
import macros.async._
import dbrary._
import site._
import models._

object AngularController extends SiteController {
  private def parseBool(s : String) : Boolean =
    !s.equals("0") && !"false".startsWith(s.toLowerCase)

  private val browserBlacklist = """Mozilla/.* \(.*\b(MSIE [0-9]\.[0-9]|AppleWebKit/.* Version/[0-5]\..* Safari/).*""".r.pattern

  def jsEnabled(implicit request : RequestHeader) : Boolean =
    request.getQueryString("js").fold(
      !request.headers.get("User-Agent").exists(browserBlacklist.matcher(_).lookingAt))(
      parseBool)

  private def publicResource(name : String) =
    Play.resourceAsStream("/public/" + name)
    .fold(
      throw new RuntimeException("missing: " + name))(
      IO.IOUtils.toString _)

  private def static[A : play.api.http.Writeable](name : String, content : => A) = {
    if (Play.isDev)
      Action(request => Ok(content))
    else {
      val data = content
      val etag = name + ":" + data.hashCode
      val now = new Timestamp
      val result = Ok(data)
        .withHeaders(
          (ETAG, HTTP.quote(etag)),
          (LAST_MODIFIED, HTTP.date(now)),
          (CACHE_CONTROL, if (Play.isProd) "public, max-age=86400" else "no-cache"))
      Action { implicit request =>
        if (HTTP.notModified(etag, now)) NotModified
        else result
      }
    }
  }

  private val constantsJson = JsonObject(
      ('messages, json.Json.toJson(Messages.messages.get("default").map(
        /* hack to fix quoting (consider using https://github.com/SlexAxton/messageformat.js if things get more complicated) */
        _.mapValues(java.text.MessageFormat.format(_)))))
    , ('permission, json.Json.toJson(Permission.values.toSeq.map(_.toString)))
    , ('release, json.Json.toJson(Release.values.toSeq.map(_.toString)))
    , ('metric, JsonRecord.map[Metric[_]](_.json)(Metric.getAll))
    , ('category, JsonRecord.map[RecordCategory](_.json)(RecordCategory.getAll))
    , ('format, JsonRecord.map[AssetFormat](_.json)(AssetFormat.getAll))
    , ('party, JsonObject(('nobody, Party.Nobody.json(AnonSite)), ('root, Party.Root.json(AnonSite))))
    , ('mode, json.JsString(current.mode.toString))
    , ('sandbox, json.JsBoolean(site.Site.sandbox))
    , ('url, json.JsString(site.Site.url))
    , ('version, json.JsString(site.Site.version))
    ).js

  private val routesJs = {
    import routes.javascript._
    play.api.Routes.javascriptRouter("routes", None, site.Site.url.stripPrefix("http://")
    , SiteHtml.start
    , LoginHtml.view
    , LoginHtml.registration
    , LoginHtml.register
    , LoginApi.get
    , LoginApi.post
    , LoginApi.logout
    , LoginApi.superuserOn
    , LoginApi.superuserOff
    , LoginApi.register
    , TokenHtml.getPassword
    , TokenHtml.issuePassword
    , TokenHtml.token
    , TokenApi.token
    , TokenApi.password
    , PartyHtml.search
    , PartyHtml.profile
    , PartyHtml.view
    , PartyHtml.edit
    , PartyHtml.avatar
    , PartyApi.profile
    , PartyApi.get
    , PartyApi.search
    , PartyApi.update
    , PartyApi.authorizeSearch
    , PartyApi.authorizeApply
    , PartyApi.authorizeChange
    , PartyApi.authorizeRemove
    , VolumeHtml.search
    , VolumeHtml.view
    , VolumeHtml.add
    , VolumeHtml.edit
    , VolumeController.thumb
    , VolumeController.zip
    , VolumeController.csv
    , VolumeApi.get
    , VolumeApi.search
    , VolumeApi.update
    , VolumeApi.create
    , VolumeController.zip
    , VolumeApi.zipList
    , VolumeApi.accessSearch
    , VolumeApi.accessChange
    , VolumeApi.accessRemove
    , VolumeApi.funderSearch
    , VolumeApi.fundingChange
    , VolumeApi.fundingRemove
    , SlotHtml.view
    , SlotHtml.edit
    , SlotController.zip
    , SlotApi.zip
    , SlotApi.get
    , SlotApi.update
    , SlotApi.create
    , SlotApi.remove
    , AssetHtml.formats
    , AssetHtml.view
    , AssetHtml.edit
    , AssetApi.get
    , AssetApi.update
    , AssetApi.uploadStart
    , AssetApi.uploadChunk
    , AssetApi.upload
    , AssetApi.replace
    , AssetApi.remove
    , AssetSlotHtml.view
    , AssetSlotController.thumb
    , AssetSlotController.download
    , AssetSlotApi.get
    , AssetSlotApi.setExcerpt
    , AssetSlotApi.removeExcerpt
    , RecordHtml.view
    , RecordHtml.edit
    , RecordApi.get
    , RecordApi.create
    , RecordApi.add
    , RecordApi.move
    , RecordApi.update
    , RecordApi.remove
    , RecordApi.measureUpdate
    , CommentApi.post
    , TagApi.search
    , TagApi.update
    , TagApi.top
    , routes.javascript.AngularController.cite
    , routes.javascript.AngularController.void
    , SiteApi.activity
    )
  }

  val constantsJs = static("constants",
    views.js.constants(constantsJson, routesJs))

  private val jsDebug = current.configuration.getBoolean("js.debug").getOrElse(false)
  private val jsLibs = Seq(
    "lib/jquery/jquery.min.js",
    "lib/angularjs/angular.min.js",
    "lib/angularjs/angular-route.min.js",
    "lib/ng-flow/ng-flow-standalone.min.js",
    "lib/lodash/lodash.min.js",
    "app.min.js",
    "templates.js")

  private lazy val jsDebugLibs = {
    import IO.filefilter.FileFilterUtils._
    import scala.collection.JavaConversions.asScalaIterator
    val dir = Play.resource("/public/app.js").flatMap(store.urlFile).get.getParentFile
    val dirp = dir.getPath + "/"
    jsLibs.takeWhile(_.startsWith("lib/")).map { js =>
      if (js.endsWith(".min.js")) js.dropRight(7) + ".js" else js
    } ++ ("debug.js" +: IO.FileUtils.iterateFiles(dir,
      and(suffixFileFilter(".js"), notFileFilter(suffixFileFilter(".min.js"))),
      notFileFilter(nameFileFilter("lib")))
      .map(_.getPath.stripPrefix(dirp)).toSeq.sorted)
  }

  def jsDepends(implicit request : RequestHeader) =
    if (Play.isDev && request.getQueryString("js.debug").fold(jsDebug)(parseBool))
      jsDebugLibs
    else jsLibs

  val page = SiteAction { implicit request =>
    Ok(views.html.angular(jsDepends))
  }

  private def analytic(data : json.JsValue)(implicit site : Site) : Future[Unit] = data match {
    case json.JsObject(f) =>
      var action : Option[Audit.Action.Value] = None
      var route : Option[String] = None
      val lb = f.genericBuilder[(String,json.JsValue)]
      f.foreach {
        case ("action", json.JsString(v)) => action = Audit.Action.withNameOpt(v)
        case ("route", json.JsString(v)) => route = Some(v)
        case kv => lb += kv
      }
      action.foreachAsync(action =>
        route.foreachAsync(route =>
          Analytic.add(action, route, json.JsObject(lb.result))))
    case _ => async.void
  }

  def analytics(implicit request : SiteRequest[_]) : Future[Unit] =
    async.when(request.isApi && request.headers.get("X-Requested-With").exists(_.equals("DatabraryClient")),
      request.headers.getAll("Analytics").foreachAsync(a =>
        scala.util.control.Exception.failAsValue[json.JsValue](classOf[com.fasterxml.jackson.core.JsonProcessingException])(json.JsUndefined("parse error"))(
          json.Json.parse(a)) match {
          case json.JsArray(l) => l.foreachAsync(analytic _)
          case j => analytic(j)
        }))


  final class CiteForm
    extends ApiForm(routes.AngularController.cite) {
    val url = Field(Forms.of[java.net.URL])
  }
  def cite = SiteAction.async { implicit request =>
    val form = new CiteForm()._bind
    Citation.get(form.url.get).map(_.fold[Result](NotFound)(c => Ok(c.json.js)))
  }

  def void =
    SiteAction { implicit request =>
      NoContent
    }
}
