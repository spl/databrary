package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import org.mindrot.jbcrypt.BCrypt
import macros._
import macros.async._
import dbrary._
import site._
import models._

sealed abstract class PartyController extends ObjectController[SiteParty] {
  /** ActionBuilder for party-targeted actions.
    * @param i target party id, defaulting to current user (site.identity)
    * @param p permission needed, or None if delegation is not allowed (must be self)
    */
  private[controllers] def action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    zip[Party.Id, Permission.Value, ActionFunction[SiteRequest.Base,Request]](i, p, (i, p) =>
      RequestObject.check[SiteParty](models.SiteParty.get(i)(_), p))
    .getOrElse {
      SiteAction.Auth andThen new ActionTransformer[SiteRequest.Auth,Request] {
        protected def transform[A](request : SiteRequest.Auth[A]) =
          if (i.forall(_ === request.identity.id))
            request.identity.perSite(request).map { p =>
              request.withObj(p)
            }
          /*
          else if (request.superuser)
            RequestObject.getter[SiteParty](models.SiteParty.get(i.get)(_)).transform[A](request)
          */
          else
            throw ForbiddenException
      }
    }

  private[controllers] def Action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    SiteAction andThen action(i, p)

  protected def searchResults(implicit request : SiteRequest[AnyContent]) : Future[Seq[Party]] = {
    val form = new PartyController.SearchForm()._bind
    val query = form.query.get
    var access = form.access.get
    var institution = form.institution.get
    if (query.isEmpty && access.isEmpty && institution.isEmpty) {
      /* set some reasonable defaults */
      access = Some(Permission.EDIT)
      institution = Some(false)
    }
    Party.search(query, access = access, institution = institution, limit = form.limit.get, offset = form.offset.get)
  }

  protected def adminAction(i : models.Party.Id, delegate : Boolean) =
    action(Some(i), if (delegate) Some(Permission.ADMIN) else None)

  protected def AdminAction(i : models.Party.Id, delegate : Boolean) =
    SiteAction andThen adminAction(i, delegate)

  protected def adminAccount(implicit request : Request[_]) : Option[Account] =
    request.obj.party.account.filter(_ === request.identity || request.superuser)

  protected def editForm(implicit request : Request[_] with AuthSite) : PartyController.EditForm =
    adminAccount.fold[PartyController.EditForm](new PartyController.PartyEditForm)(new PartyController.AccountEditForm(_))

  protected def createForm(acct : Boolean)(implicit request : SiteRequest[_]) : PartyController.CreateForm =
    if (acct) new PartyController.AccountCreateForm else new PartyController.PartyCreateForm

  def update(i : models.Party.Id) = Action(Some(i), Some(Permission.EDIT)).async { implicit request =>
    val form = editForm(request.asInstanceOf[Request[_] with AuthSite])._bind
    val party = request.obj.party
    for {
      _ <- party.change(
        sortname = form.sortname.get,
        prename = form.prename.get,
        orcid = form.orcid.get,
        affiliation = form.affiliation.get,
        url = form.url.get
      )
      _ <- form.accountForm foreachAsync { form =>
        form.checkPassword(form.account)
        form.orThrow.account.change(
          email = form.email.get,
          password = form.cryptPassword,
          openid = form.openid.get)
      }
      _ <- form.avatar.get foreachAsync { file : form.FilePart =>
        val fmt = AssetFormat.getFilePart(file).filter(_.isImage) getOrElse
          form.avatar.withError("file.format.unknown", file.contentType.getOrElse("unknown"))._throw
        request.obj.setAvatar(file.ref, fmt, Maybe(file.filename).opt())
      }
    } yield (result(request.obj))
  }

  def create(acct : Boolean = false) = SiteAction.rootAccess().async { implicit request =>
    val form = createForm(acct)._bind
    for {
      p <- Party.create(
        sortname = form.sortname.get.get,
        prename = form.prename.get.get,
        orcid = form.orcid.get.flatten,
        affiliation = form.affiliation.get.flatten,
        url = form.url.get.flatten)
      a <- cast[PartyController.AccountCreateForm](form).mapAsync(form =>
        Account.create(p,
          email = form.email.get.get,
          password = form.cryptPassword,
          openid = form.openid.get.flatten)
      )
      s <- p.perSite
    } yield (result(s))
  }

  def remove(id : Party.Id) =
    SiteAction.rootAccess().andThen(adminAction(id, true)).async { implicit request =>
      request.obj.party.remove.map { ok =>
        Ok(request.obj.party.name + (if (ok) "" else " not") + " deleted")
      }
    }

  def authorizeChange(id : models.Party.Id, childId : models.Party.Id) =
    AdminAction(id, true).async { implicit request =>
      models.Party.get(childId).flatMap(_.fold(ANotFound) { child =>
        val form = new PartyController.AuthorizeChildForm(child)._bind
        if (form.delete.get)
          models.Authorize.remove(childId, id)
            .map(_ => result(request.obj))
        else for {
          c <- Authorize.get(child, request.obj.party)
          _ <- Authorize.set(childId, id,
            form.site.get,
            form.member.get,
            form.expires.get.map(_.toLocalDateTime(new org.joda.time.LocalTime(12, 0))))
          _ <- async.when(Play.isProd && form.site.get > Permission.PUBLIC && !c.exists(_.site > Permission.PUBLIC),
            Mail.send(
              to = child.account.map(_.email).toSeq :+ Mail.authorizeAddr,
              subject = Messages("mail.authorized.subject"),
              body = Messages("mail.authorized.body", request.obj.party.name)))
        } yield (result(request.obj))
      })
    }

  def authorizeRemove(id : models.Party.Id, other : models.Party.Id) =
    AdminAction(id, true).async { implicit request =>
      for {
        /* users can remove themselves from any relationship */
        _ <- models.Authorize.remove(id, other)
        _ <- models.Authorize.remove(other, id)
      } yield (result(request.obj))
    }

  private def delegates(party : Party) : Future[Seq[Account]] =
    party.authorizeChildren().map(_.filter(_.permission >= Permission.ADMIN).flatMap(_.child.account)
      ++ party.account)

  def authorizeApply(id : models.Party.Id, parentId : models.Party.Id) =
    AdminAction(id, true).async { implicit request =>
      models.Party.get(parentId).flatMap(_.fold(ANotFound) { parent =>
      val form = new PartyController.AuthorizeApplyForm(parent)._bind
      for {
        _ <- Authorize.apply(id, parentId)
        dl <- delegates(parent)
        _ <- async.when(Play.isProd, Mail.send(
          to = dl.map(_.email) :+ Mail.authorizeAddr,
          subject = Messages("mail.authorize.subject", request.obj.party.name),
          body = Messages("mail.authorize.body", routes.PartyHtml.edit(parentId, None).absoluteURL(Play.isProd) + "?page=grant#auth-" + id,
            request.obj.party.name + request.user.fold("")(" <" + _.email + ">"),
            parent.name)
        ).recover {
          case ServiceUnavailableException => ()
        })
      } yield (result(request.obj))
      })
    }

  def authorizeSearch(id : models.Party.Id, apply : Boolean) =
    AdminAction(id, true).async { implicit request =>
      val form = new PartyController.AuthorizeSearchForm(apply)._bind
      if (form.notfound.get)
        for {
          _ <- Mail.send(
            to = Seq(Mail.authorizeAddr),
            subject = Messages("mail.authorize.subject", request.obj.party.name),
            body = Messages("mail.authorize.body", routes.PartyHtml.view(id, None).absoluteURL(Play.isProd),
              request.obj.party.name + request.user.fold("")(" <" + _.email + ">") + request.obj.party.affiliation.fold("")(" (" + _ + ")"),
              form.name.get + form.info.get.fold("")(" (" + _ + ")")))
        } yield (Ok("request sent"))
      else for {
        res <- models.Party.search(Some(form.name.get), authorize = Some(request.obj.party), institution = form.institution.get)
        r <- if (request.isApi) async(Ok(JsonArray.map[Party, JsonRecord](_.json)(res)))
          else PartyHtml.viewAdmin(form +: res.map(e =>
            (if (apply) new PartyController.AuthorizeApplyForm(e) else new PartyController.AuthorizeChildForm(e))))
            .map(Ok(_))
      } yield (r)
    }
}

object PartyController extends PartyController {
  final class SearchForm(implicit request : SiteRequest[_])
    extends HtmlForm[SearchForm](
      routes.PartyHtml.search(),
      _ => views.html.party.search(Nil))
    with NoCsrfForm {
    val query = Field(Mappings.maybeText)
    val access = Field(OptionMapping(Mappings.enum(Permission)))
    val institution = Field(OptionMapping(Forms.boolean))
    val limit = Field(Forms.default(Forms.number(1,129),25))
    val offset = Field(Forms.default(Forms.number(0),0))
  }

  abstract sealed class PartyForm(action : Call)(implicit request : SiteRequest[_])
    extends HtmlForm[PartyForm](action,
      views.html.party.edit(_)) {
    def actionName : String
    def formName : String = actionName + " Party"
    val sortname : Field[Option[String]]
    val prename = Field(OptionMapping(Mappings.maybeText))
    val orcid = Field(OptionMapping(Forms.optional(Forms.of[Orcid])))
    val affiliation = Field(OptionMapping(Mappings.maybeText))
    val url = Field(OptionMapping(Forms.optional(Forms.of[java.net.URL])))
  }
  sealed trait AccountForm extends PartyForm with LoginController.PasswordChangeForm {
    override def formName : String = actionName + " Account"
    val email : Field[Option[String]]
    val password = passwordField.fill(None)
    val openid = Field(OptionMapping(Forms.optional(Forms.of[java.net.URL])))
  }

  abstract sealed class EditForm(implicit request : Request[_])
    extends PartyForm(routes.PartyHtml.update(request.obj.id)) {
    def actionName = "Update"
    override def formName = "Edit Profile"
    def party = request.obj.party
    def accountForm : Option[AccountEditForm]
    val sortname = Field(OptionMapping(Mappings.nonEmptyText)).fill(Some(party.sortname))
    val avatar = OptionalFile()
    prename.fill(Some(party.prename))
    orcid.fill(Some(party.orcid))
    affiliation.fill(Some(party.affiliation))
    url.fill(Some(party.url))
  }
  final class PartyEditForm(implicit request : Request[_]) extends EditForm {
    def accountForm = None
  }
  final class AccountEditForm(val account : Account)(implicit protected val request : Request[_] with AuthSite) extends EditForm with AccountForm with LoginController.AuthForm {
    def accountForm = if (_authorized) Some(this)
      else if (email.get.exists(!_.equals(account.email)) || password.get.nonEmpty || openid.get.exists(!_.equals(account.openid)))
        Some(this.auth.withError("error.required"))
      else None
    val email = Field(OptionMapping(Forms.email)).fill(Some(account.email))
    openid.fill(Some(account.openid))
  }

  abstract sealed class CreateForm(implicit request : SiteRequest[_])
    extends PartyForm(routes.PartyHtml.create()) {
    def actionName = "Create"
    val sortname = Field(Mappings.some(Mappings.nonEmptyText))
  }
  final class PartyCreateForm(implicit request : SiteRequest[_]) extends CreateForm
  final class AccountCreateForm(implicit request : SiteRequest[_]) extends CreateForm with AccountForm {
    val email = Field(Mappings.some(Forms.email))
  }

  private final val maxExpiration = org.joda.time.Years.years(2)

  sealed trait AuthorizeBaseForm extends StructForm
  sealed trait AuthorizeFullForm extends AuthorizeBaseForm {
    val site = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    val member = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    val delete = Field(Forms.boolean).fill(false)
    val expires = Field(Forms.optional(Forms.jodaLocalDate))
    def pending = site.get != Permission.NONE || member.get != Permission.NONE
    private[controllers] def _fill(auth : Authorize) : this.type = {
      site.fill(auth.site)
      member.fill(auth.member)
      expires.fill(auth.expires.map(_.toLocalDate))
      this
    }
  }
  sealed abstract class AuthorizeForm(action : Call)(implicit request : Request[_])
    extends AHtmlForm[AuthorizeForm](action,
      f => PartyHtml.viewAdmin(Seq(f)))
    with AuthorizeBaseForm {
    def _apply : Boolean
  }
  sealed trait AuthorizeOtherForm extends AuthorizeForm {
    def targetParty : Party
  }
  final class AuthorizeChildForm(val child : Party)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeChange(request.obj.id, child.id))
    with AuthorizeFullForm
    with AuthorizeOtherForm {
    def targetParty = child
    def _apply = false
    private[this] val maxexp = (new Date).plus(maxExpiration)
    override val expires = Field(if (request.superuser) Forms.optional(Forms.jodaLocalDate)
      else Mappings.some(Forms.default(Forms.jodaLocalDate, maxexp)
        .verifying(validation.Constraint[Date]("constraint.max", maxExpiration) { d =>
          if (d.isAfter(maxexp) || d.isBefore(new Date(2000,1,1))) validation.Invalid(validation.ValidationError("auth.grant.expires.help", maxExpiration))
          else validation.Valid
        }), maxexp))
    private[controllers] override def _fill(auth : Authorize) : this.type = {
      assert(request.obj === auth.parent)
      assert(child === auth.child)
      super._fill(auth)
    }
  }
  final class AuthorizeApplyForm(val parent : Party)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeApply(request.obj.id, parent.id))
    with AuthorizeOtherForm {
    def targetParty = parent
    def _apply = true
  }
  final class AuthorizeSearchForm(val _apply : Boolean)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeSearch(request.obj.id, _apply)) {
    val name = Field(Mappings.nonEmptyText)
    val institution = Field(OptionMapping(Forms.boolean)).fill(None)
    val notfound = Field(Forms.boolean).fill(false)
    val info = Field(Mappings.maybeText)
  }
  final class AuthorizeAdminForm(val authorize : Authorize)(implicit request : SiteRequest[_])
    extends StructForm(routes.PartyHtml.authorizeChange(authorize.parentId, authorize.childId))
    with AuthorizeFullForm {
    _fill(authorize)
  }
}

object PartyHtml extends PartyController with HtmlController {
  import PartyController._

  def viewParty(implicit request : Request[_]) =
    for {
      parents <- request.obj.party.authorizeParents()
      children <- request.obj.party.authorizeChildren()
      vols <- request.obj.volumeAccess
      comments <- request.obj.party.account.fold[Future[Seq[Comment]]](async(Nil))(_.comments)
    } yield (Ok(views.html.party.view(parents, children, vols, comments)))

  def profile(js : Option[Boolean]) =
    SiteAction.js.andThen(action(None, Some(Permission.NONE))).async(viewParty(_))

  def view(i : models.Party.Id, js : Option[Boolean]) =
    SiteAction.js.andThen(action(Some(i), Some(Permission.NONE))).async(viewParty(_))

  def search(js : Option[Boolean]) =
    SiteAction.js.async { implicit request =>
      searchResults.map(l => Ok(views.html.party.search(l)))
    }

  private[controllers] def viewAdmin(
    authorizeForms : Seq[AuthorizeForm] = Nil)(
    implicit request : Request[_]) = {
    val change = authorizeForms.collect { case o : AuthorizeOtherForm => o.targetParty.id }.toSet
    val search = Set(false, true) -- authorizeForms.collect { case a : AuthorizeSearchForm => a._apply }
    for {
      parents <- request.obj.party.authorizeParents(true)
      children <- request.obj.party.authorizeChildren(true)
      forms = children
        .filterNot(t => change.contains(t.childId))
        .map(t => new AuthorizeChildForm(t.child)._fill(t)) ++
        search.map(new AuthorizeSearchForm(_)) ++
        authorizeForms
    } yield (views.html.party.authorize(parents, forms))
  }

  def edit(i : models.Party.Id, js : Option[Boolean]) =
    SiteAction.js.andThen(adminAction(i, true)).async { implicit request =>
      editForm(request.asInstanceOf[Request[_] with AuthSite]).Ok
    }

  def createNew(acct : Boolean = false) = SiteAction.rootAccess().async { implicit request =>
    createForm(acct).Ok
  }

  def preRemove(id : Party.Id) =
    SiteAction.rootAccess().andThen(adminAction(id, true)) { implicit request =>
      Ok(views.html.party.remove(request.obj))
    }

  def admin(i : models.Party.Id) = AdminAction(i, false).async { implicit request =>
    viewAdmin().map(Ok(_))
  }

  def authorizeAdmin = SiteAction.rootMember().async { implicit request =>
    for {
      all <- Authorize.getAll
      (rest, pend) = all.partition(_.authorized)
      (act, exp) = rest.partition(_.valid)
      part <- Party.getAll
    } yield (Ok(views.html.party.authorizeAdmin(part, pend.map(new AuthorizeAdminForm(_)), act, exp)))
  }

  /** Resend the investigator agreement through Mail.investigator. */
  def investigator(i : models.Party.Id) =
    SiteAction.rootMember().andThen(action(Some(i))).async { implicit request =>
      Mail.investigator(request.obj.party).map(HTTP.wsResult)
    }

  /** Issue a new password reset token with the "reissue" message. */
  def reissue(i : models.Account.Id) =
    SiteAction.rootAccess().andThen(action(Some(i))).async { implicit request =>
      request.obj.party.account.fold(ANotFound) { a =>
        TokenHtml.newPassword(Right(a), "reissue").map { t =>
          Ok(if (request.superuser) "sent: " + t.fold("none")(_.id) else "sent")
        }
      }
    }

  def avatar(i : models.Party.Id, size : Int = 64) =
    SiteAction.async { implicit request =>
      FileAsset.getAvatar(i).flatMap(_.fold(
        async(Found("/public/images/avatar.png")))(
        AssetController.assetResult(_, Some(size))))
    }

}

object PartyApi extends PartyController with ApiController {
  def get(partyId : models.Party.Id) = Action(Some(partyId), Some(Permission.NONE)).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  def profile =
    SiteAction.andThen(action(None, Some(Permission.NONE))).async { implicit request =>
      request.obj.json(request.apiOptions).map(Ok(_))
    }

  def search =
    SiteAction.async { implicit request =>
      searchResults.map(l => Ok(JsonArray.map[Party, JsonRecord](_.json)(l)))
    }

}
