package controllers

import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import          libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import macros._
import macros.async._
import site._
import dbrary._
import models._

private[controllers] sealed class SlotController extends ObjectController[Slot] {
  private[controllers] def action(i : Container.Id, segment : Segment, p : Permission.Value = Permission.VIEW) =
    RequestObject.check(Slot.get(i, segment)(_), p)

  private[controllers] def Action(i : Container.Id, segment : Segment, p : Permission.Value = Permission.VIEW) =
    SiteAction andThen action(i, segment, p)

  def update(i : Container.Id, segment : Segment) =
    Action(i, segment, Permission.EDIT).async { implicit request =>
      val form = SlotController.editForm._bind
      for {
        cont <- cast[SlotController.ContainerEditForm](form).fold(async(request.obj))(form =>
          request.obj.container.change(name = form.name.get, date = form.date.get))
        _ <- form.release.get.foreachAsync((c : Release.Value) => cont.setRelease(c).map(r =>
            if (!r) form.release.withError("error.conflict")._throw))
        /* refresh release: */
        s <- Slot.get(i, segment)
      } yield (result(s.get))
    }

  def create(v : models.Volume.Id) =
    VolumeController.Action(v, Permission.CONTRIBUTE).async { implicit request =>
      val form = new SlotController.ContainerCreateForm()._bind
      for {
        cont <- models.Container.create(request.obj, name = form.name.get.flatten, date = form.date.get.flatten, top = form.top.get)
        _ <- form.release.get.foreachAsync((c : Release.Value) => cont.setRelease(c))
      } yield (result(cont))
    }
}

object SlotController extends SlotController {
  sealed trait SlotForm extends HtmlFormView {
    def actionName : String
    def formName : String = actionName + " Session"

    val release = Field(OptionMapping(Mappings.enum(Release)))
  }
  sealed trait ContainerForm extends SlotForm {
    val name = Field(OptionMapping(Mappings.maybeText))
    val date = Field(OptionMapping(Forms.optional(Forms.jodaLocalDate)))
  }

  sealed class EditForm(implicit request : Request[_])
    extends AHtmlForm[EditForm](
      routes.SlotHtml.update(request.obj.containerId, request.obj.segment),
      f => SlotHtml.viewEdit(Some(f)))
    with SlotForm {
    def actionName = "Update"
    override def formName = "Edit Session"
    release.fill(Some(request.obj.release))
  }
  final class ContainerEditForm(implicit request : Request[_])
    extends EditForm with ContainerForm {
    name.fill(Some(request.obj.container.name))
    date.fill(Some(request.obj.container.date))
  }
  def editForm(implicit request : Request[_]) : EditForm =
    if (request.obj.isFull) new ContainerEditForm
    else new EditForm

  final class ContainerCreateForm(implicit request : VolumeController.Request[_])
    extends HtmlForm[ContainerCreateForm](
      routes.SlotHtml.create(request.obj.id),
      views.html.slot.edit(_, Nil, None))
    with ContainerForm {
    val top = Field(Forms.boolean).fill(false)
    def actionName = "Create"
  }

  def zip(v : Volume.Id, i : Container.Id, segment : Segment) = Action(i, segment).async { implicit request =>
    request.obj.auditDownload.map { _ =>
      AssetController.zipResult(store.Zip.slot(request.obj), "databrary-" + request.obj.volumeId + "-" + request.obj.containerId + request.obj.pageCrumbName.fold("")("-" + _))
    }
  }
}

object SlotHtml extends SlotController with HtmlController {
  import SlotController._

  private[controllers] def show(commentForm : Option[CommentController.SlotForm] = None, tagForm : Option[TagController.SlotForm] = None)(implicit request : Request[_]) = {
    val slot = request.obj
    for {
      records <- slot.records
      assets <- SlotAsset.getSlot(slot)
      comments <- slot.comments
      tags <- TagWeight.getSlot(slot)
    } yield (views.html.slot.view(records, assets, comments, commentForm.getOrElse(new CommentController.SlotForm), tags, tagForm.getOrElse(new TagController.SlotForm)))
  }

  def view(v : Volume.Id, i : Container.Id, segment : Segment, js : Option[Boolean]) =
    SiteAction.js.andThen(action(i, segment)).async { implicit request =>
      show().map(Ok(_))
    }

  private[controllers] def viewEdit(form : Option[EditForm] = None, recordForm : Option[RecordController.SelectForm] = None)(implicit request : Request[_]) =
    for {
      records <- request.obj.records
      all <- request.obj.volume.records
      selectList = all diff records
    } yield (views.html.slot.edit(form getOrElse editForm, records, recordForm orElse Some(new RecordController.SelectForm), selectList))

  def edit(v : Volume.Id, i : Container.Id, segment : Segment, js : Option[Boolean]) =
    SiteAction.js.andThen(action(i, segment, Permission.EDIT)).async { implicit request =>
      editForm.Ok
    }

  def add(v : models.Volume.Id) =
    VolumeController.Action(v, Permission.EDIT).async { implicit request =>
      new ContainerCreateForm().Ok
    }
}

object SlotApi extends SlotController with ApiController {
  def get(v : models.Volume.Id, c : models.Container.Id, segment : Segment) = Action(c, segment).async { request =>
    request.obj.slotJson(request.apiOptions).map(Ok(_))
  }

  def remove(i : Container.Id) =
    SiteAction.andThen(RequestObject.check(Container.get(i)(_), Permission.EDIT)).async { implicit request =>
      request.obj.remove.map { r =>
        if (r) NoContent else Conflict(request.obj.json)
      }
    }

  def zipList(v : Volume.Id, i : Container.Id, segment : Segment) = Action(i, segment).async { implicit request =>
    store.Zip.slotAssetList(request.obj).map(x => Ok(JsObject(x)))
  }
}
