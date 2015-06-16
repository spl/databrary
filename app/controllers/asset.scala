package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import          libs.Files.TemporaryFile
import          libs.iteratee.{Iteratee,Enumerator}
import          libs.concurrent.Execution.Implicits.defaultContext
import          libs.Crypto
import java.io.RandomAccessFile
import macros._
import macros.async._
import dbrary._
import site._
import models._

private[controllers] sealed class AssetController extends ObjectController[Asset] {
  protected def action(i : models.Asset.Id, p : Permission.Value) =
    RequestObject.check(models.Asset.get(i)(_), p)

  protected def Action(i : models.Asset.Id, p : Permission.Value) =
    SiteAction andThen action(i, p)

  private[this] def duration(asset : Asset) : Future[Offset] =
    asset.slot.map { sa =>
      sa.flatMap(_.segment.zip((l, u) => u - l)).filter(_ != Offset.ZERO) getOrElse asset.duration
    }

  private[this] def set(asset : Asset, form : AssetController.AssetForm, autopos : Boolean = false)(implicit request : SiteRequest[_]) =
    for {
      slot <- form.container.get.mapAsync { c =>
        form.position.get.orElseAsync(
          if (autopos) SlotAsset.containerEnd(c).map(e => Some(e.getOrElse(Offset.ZERO)))
          else async(None))
        .flatMap(_.fold(async(Segment.full))(p =>
          duration(asset).map(d => Segment(p, p + d))))
        .flatMap(Slot.get(c, _))
        .map(_ getOrElse form.container.withError("object.invalid", "container")._throw)
      }
      _ <- asset.change(
        name = form.name.get.map(_.map(asset.format.stripExtension)),
        release = form.classification.get)
      _ <- slot.foreachAsync(asset.link)
      _ <- form.excerpt.get.foreachAsync(e => Excerpt.set(asset, Segment.full, e).map(r =>
          if (!r && e.nonEmpty) form.excerpt.withError("error.conflict")._throw))
      /* refresh excerpt: */
      sa <- asset.slot
    } yield (sa.fold(result(asset))(AssetSlotController.result _))

  private[this] def checkSuperseded(form : FormView)(implicit request : Request[_]) : Future[Unit] =
    request.obj.isSuperseded.map(when(_,
      throw form.withGlobalError("file.superseded")._exception(Results.Conflict)))

  def update(o : models.Asset.Id) =
    Action(o, Permission.EDIT).async { implicit request =>
      val form = new AssetController.ChangeForm()._bind
      for {
        _ <- checkSuperseded(form)
        r <- set(request.obj, form)
      } yield (r)
    }

  private def create(form : AssetController.AssetUploadForm) : Future[Asset] = {
    implicit val request = form.request
    val adm = request.access.isAdmin
    val ifmt = form.format.get.filter(_ => adm).flatMap(AssetFormat.get(_))
    for {
      (file, ftype, fname) <- (form.file.get, form.upload.get, form.localfile.get) match {
        case (Some(file), None, None) =>
          async((file.ref, file.contentType, Maybe(file.filename).opt()))
        case (None, Some(token), None) =>
          UploadToken.take(token)(request.asInstanceOf[AuthSite])
          .map(_.fold(form.upload.withError("file.notfound")._throw) { u =>
            (TemporaryFile(u.file), None, Some(u.filename))
          })
        case (None, None, Some(localfile)) if adm =>
          /* local file handling, for admin only: */
          val file = store.Stage.file(localfile)
          val name = file.getName
          if (!file.isFile)
            form.localfile.withError("file.notfound")._throw
          async((store.TemporaryFileLinkOrCopy(file), None, Some(name)))
        case _ =>
          /* or, like, just conflicting options, whatever: */
          form.file.withError("error.required")._throw
      }
      _ <- when(file.file.length <= 0,
        form.file.withError("file.size.invalid")._throw)
      fmt = ifmt
        .orElse(ftype.flatMap(AssetFormat.getMimetype(_)))
        .orElse(fname.flatMap(AssetFormat.getFilename(_)))
        .getOrElse(form.format.withError("file.format.unknown")._throw)
      name = fname.map(fmt.stripExtension)
      classification = form.classification.get.getOrElse(Release.PRIVATE)
      asset <- fmt match {
        case fmt : TimeseriesFormat if adm && form.timeseries.get =>
          models.TimeseriesAsset.create(form.volume, fmt, classification, name, file)
        case fmt if fmt.isTranscodable.nonEmpty =>
          val av = try {
            media.AV.probe(file.file)
          } catch { case e : media.AV.Error =>
            form.file.withError("file.invalid", e.getMessage)._throw
          }
          models.FileAsset.create(form.volume, fmt, classification, name, file)
          .flatMap(Transcode.create(_, duration = av.duration)
            .whenSuccess(_.start)
            .map(_.asset))
        case fmt =>
          models.FileAsset.create(form.volume, fmt, classification, name, file)
      }
    } yield asset
  }

  def upload(v : models.Volume.Id) =
    VolumeHtml.Action(v, Permission.CONTRIBUTE).async { implicit request =>
      val form = new AssetController.UploadForm()._bind
      create(form).flatMap(a => set(a, form, a.duration > Offset.ZERO))
    }

  def replace(o : models.Asset.Id) =
    Action(o, Permission.CONTRIBUTE).async { implicit request =>
      val form = new AssetController.ReplaceForm()._bind
      for {
        _ <- checkSuperseded(form)
        asset <- create(form)
        _ <- asset.supersede(request.obj)
        r <- set(asset, form)
      } yield (r)
    }

  def remove(a : models.Asset.Id) = Action(a, Permission.EDIT).async { implicit request =>
    for {
      _ <- request.obj.unlink
    } yield (result(request.obj))
  }
}

object AssetController extends AssetController {
  sealed abstract class AssetForm(action : Call)(implicit val request : RequestObject.Site[_])
    extends HtmlForm[AssetForm](action,
      views.html.asset.edit(_)) {
    def actionName : String
    def formName : String = actionName + " Asset"
    def volume : Volume

    val name = Field(OptionMapping(Mappings.maybeText))
    val classification = Field(OptionMapping(Mappings.enum(Release)))
    val container = Field(Forms.optional(Forms.of[Container.Id]))
    val position = Field(Forms.optional(Forms.of[Offset]))
    val excerpt = Field(OptionMapping(Forms.optional(Mappings.enum(Release))))
  }

  sealed trait AssetUpdateForm extends AssetForm {
    def asset : Asset
    def volume = asset.volume
    name.fill(Some(asset.name))
    classification.fill(Some(asset.release))
    container.fill(None)
    position.fill(None)
  }

  final class ChangeForm(implicit request : Request[_])
    extends AssetForm(routes.AssetHtml.update(request.obj.id))
    with AssetUpdateForm {
    def actionName = "Update"
    override def formName = "Edit Asset"
    def asset = request.obj
  }

  sealed trait AssetUploadForm extends AssetForm {
    val format = Field(Forms.optional(Forms.of[AssetFormat.Id]))
    val timeseries = Field(Forms.boolean)
    val localfile = Field(Forms.optional(Forms.nonEmptyText))
    val upload = Field(Forms.optional(Forms.nonEmptyText))
    val file = OptionalFile()
  }

  final class UploadForm(implicit request : VolumeController.Request[_])
    extends AssetForm(routes.AssetHtml.upload(request.obj.id))
    with AssetUploadForm {
    def actionName = "Upload"
    def volume = request.obj
    classification.fill(Some(Release.DEFAULT))
  }

  final class ReplaceForm(implicit request : Request[_])
    extends AssetForm(routes.AssetHtml.replace(request.obj.id))
    with AssetUpdateForm with AssetUploadForm {
    def actionName = "Replace"
    def asset = request.obj
  }

  final val defaultThumbSize : Int = 480

  private[controllers] def assetResult(asset : BackedAsset, size : Option[Int] = None, saveAs : Option[String] = None)(implicit request : SiteRequest[_]) : Future[Result] = {
    val tag = asset.etag
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    if (HTTP.notModified(tag, new Timestamp(0)))
      async(NotModified)
    else for {
      data <- store.Asset.read(asset, size)
      date = store.Asset.timestamp(asset)
    } yield {
      val size = data.size
      val range = if (request.headers.get(IF_RANGE).forall(HTTP.unquote(_).equals(tag)))
          request.headers.get(RANGE).flatMap(HTTP.parseRange(_, size))
        else
          None
      val subdata = range.fold(data)((data.range _).tupled)
      val headers = Seq[Option[(String, String)]](
        Some(CONTENT_LENGTH -> subdata.size.toString),
        Some(ACCEPT_RANGES -> "bytes"),
        range.map(r => CONTENT_RANGE -> ("bytes " + (if (r._1 >= size) "*" else r._1.toString + "-" + r._2.toString) + "/" + size.toString)),
        Some(CONTENT_TYPE -> asset.format.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + asset.format.extension.fold("")("." + _)))),
        Some(LAST_MODIFIED -> HTTP.date(new Timestamp(date))),
        Some(ETAG -> HTTP.quote(tag)),
        Some(CACHE_CONTROL -> "max-age=31556926, private") /* this needn't be private for public data */
      ).flatten
        Result(
          header = ResponseHeader(range.fold(OK)(r => if (r._1 >= size) REQUESTED_RANGE_NOT_SATISFIABLE else PARTIAL_CONTENT),
            Map(headers : _*)),
          body = subdata)
      }
  }

  private[controllers] def zipResult(zip : Enumerator[Array[Byte]], name : String) : Result =
    Result(
      header = ResponseHeader(OK, Map(
        CONTENT_TYPE -> "application/zip",
        CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + ".zip")),
        CACHE_CONTROL -> "max-age=31556926, private")),
      body = zip)
}

object AssetHtml extends AssetController with HtmlController {
  import AssetController._

  def view(o : models.Asset.Id) = Action(o, Permission.VIEW).async { implicit request =>
    request.obj.slot.map(_.fold[Result](
      throw NotFoundException /* TODO */)(
      sa => Redirect(sa.pageURL)))
  }

  def edit(o : models.Asset.Id) =
    Action(o, Permission.EDIT).async { implicit request =>
      request.obj.slot.flatMap { sa =>
        val form = new ChangeForm()
        form.excerpt.fill(Some(sa.flatMap(_.excerpt.map(_.release))))
        form.Ok
      }
    }

  def replaceView(o : models.Asset.Id) =
    Action(o, Permission.EDIT).async { implicit request =>
      new ReplaceForm().Ok
    }

  def create(v : models.Volume.Id, c : Option[Container.Id], pos : Option[Offset]) =
    VolumeHtml.Action(v, Permission.CONTRIBUTE).async { implicit request =>
      val form = new UploadForm()
      form.container.fill(c)
      form.position.fill(pos)
      form.Ok
    }

  final class TranscodeForm(val transcode : Transcode)(implicit val request : SiteRequest[_])
    extends HtmlForm[TranscodeForm](routes.AssetHtml.transcode(transcode.id),
      f => views.html.asset.transcodes(Some(f))) {
    val start = Field(Forms.optional(Forms.of[Offset])).fill(transcode.segment.lowerBound)
    val end = Field(Forms.optional(Forms.of[Offset])).fill(transcode.segment.upperBound)
    val stop = Field(Forms.boolean).fill(false)
  }

  private def getTranscode(id : Asset.Id)(implicit site : Site) : Future[Option[Transcode]] =
    Transcode.get(id).orElseAsync(Asset.get(id).flatMapAsync {
      case a : FileAsset =>
        for {
          revs <- Asset.getRevisions(a)
          o = if (revs.nonEmpty) revs.minBy(_._id) else a
          s <- a.slot.flatMapAsync(s => s.releases.map(_.headOption.flatMap { c =>
            for {
              l <- s.segment.lowerBound
              u <- s.segment.upperBound
            } yield (Range(c.segment.lowerBound.filter(_ > l), c.segment.upperBound.filter(_ < u)).map(_ - l))
          }))
        } yield Some(Transcode(o, s.getOrElse(Segment.full)))
      case _ => async(None)
    })

  private def transcoding_(id : Option[Asset.Id]) =
    SiteAction.rootMember().async { implicit request =>
      id.fold[Future[Iterable[Transcode]]](Transcode.getActive)(getTranscode(_).map(_.toIterable))
      .map(t => Ok(views.html.asset.transcodes(t.map(new TranscodeForm(_)))))
    }

  def transcodingAll = transcoding_(None)
  def transcoding(id : Asset.Id) = transcoding_(Some(id))

  def transcode(id : Asset.Id) =
    SiteAction.rootMember().async { implicit request =>
      for {
        mt <- getTranscode(id)
        t = mt.getOrElse(throw NotFoundException)
        form = new TranscodeForm(t)._bind
        t <- t match {
          case t : TranscodeJob => async(t)
          case t if !form.stop.get => Transcode.create(t.orig, Range(form.start.get, form.end.get))
          case _ => throw NotFoundException
        }
        _ <-
          if (form.stop.get) t.stop
          else t.start
      } yield Ok(views.html.asset.transcodes(Some(new TranscodeForm(t))))
    }

  def formats(js : Option[Boolean]) = SiteAction.js { implicit request =>
    Ok(views.html.asset.formats(
      AssetFormat.getAll.toSeq.groupBy(_.mimeSubTypes._1)))
  }
}

object AssetApi extends AssetController with ApiController {
  def get(i : models.Asset.Id) = Action(i, Permission.VIEW).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  private class StartForm(volumeId : Volume.Id) extends ApiForm(routes.AssetApi.uploadStart(volumeId)) {
    val filename = Field(Forms.text)
    val size = Field(Forms.longNumber(1))
  }

  private val logger = play.api.Logger("upload")

  def uploadStart(v : Volume.Id) =
    VolumeApi.Action(v, Permission.EDIT).async { implicit request =>
      val form = new StartForm(v)._bind
      if (AssetFormat.getFilename(form.filename.get).isEmpty)
        form.filename.withError("file.format.unknown")._throw
      for {
        u <- UploadToken.create(form.filename.get)(request.asInstanceOf[AuthSite])
      } yield {
        val f = new RandomAccessFile(u.file, "rw")
        try {
          f.setLength(form.size.get)
        } finally {
          f.close
        }
        Ok(u.id)
      }
    }

  private class ChunkForm extends ApiForm(routes.AssetApi.uploadChunk) {
    val flowChunkNumber = Field(Forms.number(1))
    val flowCurrentChunkSize = Field(Mappings.option(Forms.number(1)))
    val flowChunkSize = Field(Forms.number(1024))
    val flowTotalSize = Field(Forms.longNumber(1))
    val flowIdentifier = Field(Forms.text)
    val flowFilename = Field(Forms.text)
  }

  private def uploadChunkPrepare[A](notfound : => A, write : Boolean = true)(run : (RandomAccessFile, Int) => A = ((f : RandomAccessFile, _ : Int) => f))(implicit request : play.api.mvc.Request[AnyContent] with AuthSite) : Future[A] = {
    val form = new ChunkForm()._bind
    UploadToken.get(form.flowIdentifier.get)
    .map(_.filter(_.filename == form.flowFilename.get).fold(notfound) { u =>
      val f = new RandomAccessFile(u.file, if (write) "rw" else "r")
      if (f.length != form.flowTotalSize.get)
        form.flowTotalSize.withError("size mismatch")._throw
      f.seek(form.flowChunkSize.get.toLong * (form.flowChunkNumber.get-1).toLong)
      run(f, form.flowCurrentChunkSize.get.getOrElse(form.flowChunkSize.get))
    })
  }

  def uploadChunk = DeferredAction.site(SiteAction.auth) { implicit request =>
    Iteratee.flatten {
      uploadChunkPrepare(Iteratee.ignore[Array[Byte]].map[Result](_ => NotFound), true) { (f, z) =>
        var t = 0
        Iteratee.foreach[Array[Byte]] { b =>
          f.write(b)
          t += b.length
        }.map[Result] { _ =>
          f.close
          if (t == z)
            NoContent
          else {
            logger.error("wrong size chunk " + t + "/" + z)
            BadRequest("Wrong size upload: file being uploaded may have moved or changed")
          }
        }
        .recover[Result] { case e : Throwable =>
          f.close
          logger.error("uploading", e)
          throw e
        }
      }
    }
  }

  /* Kind of hacky, but we assume a chunk has not been uploaded if the underlying file block is all 0.
   * But worst case, people end up uploading all-0 chunks of their file again.
   */
  def uploadChunkTest = SiteAction.auth.async { implicit request =>
    uploadChunkPrepare[Result](NotFound, false) { (f, z) =>
      @scala.annotation.tailrec def test(z : Int) : Result = {
        if (z == 0)
          return ResetContent
        val x = new Array[Byte](z)
        val r = f.read(x)
        if (x.take(r).exists(_ != 0))
          return NoContent
        test(z-r)
      }
      test(z)
    }
  }

  class TranscodedForm(id : Transcode.Id) extends {
      val token = new TokenAuth {
        val token = id.toString
      }
    } with StructForm(routes.AssetApi.transcoded(id, token.auth())) with NoCsrfForm {
    val pid = Field(Forms.number)
    val res = Field(Forms.number)
    val sha1 = Field(OptionMapping(Mappings.hash(store.SHA1, store.Hex)))
    val log = Field(Forms.text)
  }

  /** Called from remote transcoding process only. */
  def transcoded(i : models.Transcode.Id, auth : String) =
    play.api.mvc.Action.async { implicit request =>
      val form = new TranscodedForm(i)._bind
      if (!form.token.checkAuth(auth) || form.hasErrors)
        async(BadRequest("error"))
      else for {
        tc <- models.Transcode.get(i)
        job = tc.filter(_.process.exists(_ == form.pid.get)).getOrElse(throw NotFoundException)
        a <- job.collect(form.res.get, form.sha1.get, form.log.get)
      } yield (Ok(a.id.toString))
    }
}
