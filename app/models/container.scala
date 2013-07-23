package models

import anorm._
import anorm.SqlParser.scalar
import dbrary._
import dbrary.Anorm._
import util._

sealed abstract class Container protected (val id : Container.Id) extends TableRowId[Container] with CommentPage {
  /* Study owning this container (possibly itself) */
  def study : Study
  def permission : Permission.Value = study.permission

  def objects(implicit db : Site.DB) = ObjectLink.getObjects(this)
  def getObject(o : Object.Id)(implicit db : Site.DB) = ObjectLink.get(this, o)

  def comments(only : Boolean = false)(implicit db : Site.DB) = Comment.getContainer(this, only)(db)
  def addComment(text : String)(implicit site : Site) = Comment.create(this, text)
}

final class Study private (override val id : Study.Id, title_ : String, description_ : Option[String], override val permission : Permission.Value) extends Container(id) with TableRowId[Study] {
  def study = this

  private[this] var _title = title_
  def title = _title
  private[this] var _description = description_
  def description = _description

  def change(title : String = _title, description : Option[String] = _description)(implicit site : Site) : Unit = {
    if (title == _title && description == _description)
      return
    val args = Anorm.Args('id -> id, 'title -> title, 'description -> description)
    Audit.SQLon(AuditAction.change, "study", "SET title = {title}, description = {description} WHERE id = {id}")(args : _*).execute()(site.db)
    _title = title
    _description = description
  }

  def pageName(implicit site : Site) = title
  def pageParent(implicit site : Site) = None
  def pageURL = controllers.routes.Study.view(id).url

  def entityAccess(p : Permission.Value = Permission.NONE)(implicit db : Site.DB) = StudyAccess.getEntities(this, p)

  def slots(implicit db : Site.DB) = Slot.getStudy(this)
  def slot(ident : String)(implicit db : Site.DB) = Slot.getIdent(this, ident)
}

final class Slot private (override val id : Slot.Id, val study : Study, ident_ : String) extends Container(id) with TableRowId[Slot] {
  def studyId = study.id
  private[this] var _ident = ident_
  def ident = _ident

  def change(ident : String = _ident)(implicit site : Site) : Boolean = {
    if (ident == _ident)
      return true
    try {
      Audit.SQLon(AuditAction.change, "slot", "SET ident = {ident} WHERE id = {id}")('id -> id, 'ident -> ident).execute()(site.db)
      _ident = ident
      true
    } catch {
      case e : java.sql.SQLException if e.getMessage.startsWith("ERROR: duplicate key value violates unique constraint \"slot_study_ident_key\"") => false
      case _ : java.sql.SQLIntegrityConstraintViolationException => false
    }
  }

  def pageName(implicit site : Site) = ident
  def pageParent(implicit site : Site) = Some(study)
  def pageURL = controllers.routes.Slot.view(id).url
}


private[models] sealed abstract class ContainerView[R <: Container with TableRowId[R]](table : String) extends TableViewId[R](table) {
  protected final val permission = "study_access_check(study.id, {identity})"
  private[models] final val condition = permission + " >= 'VIEW'"

  def get(i : Id)(implicit site : Site) : Option[R]
}

object Container extends ContainerView[Container]("container") {
  private[models] val row =
    (Study.row ~ Slot.baseRow.?) map {
      case (study ~ None) => study
      case (study ~ Some(slot)) => Slot.baseMake(study)(slot)
    }
  private[models] override val * = Study.* + ", " + Slot.*
  private[models] override val src = "container LEFT JOIN slot USING (id) JOIN study ON study.id = container.id OR study.id = slot.study"
  def get(i : Id)(implicit site : Site) : Option[Container] =
    SQL("SELECT " + * + " FROM " + src + " WHERE container.id = {id} AND " + condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
}

object Study extends ContainerView[Study]("study") {
  private[this] def make(id : Id, title : String, description : Option[String], permission : Option[Permission.Value] = None) =
    new Study(id, title, description, permission.getOrElse(Permission.NONE))
  private[models] val row = Anorm.rowMap(make _, col("id"), col("title"), col("description"), "permission")
  private[models] override val * = col("*") + ", " + permission + " AS permission"

  def get(i : Id)(implicit site : Site) : Option[Study] =
    SQL("SELECT " + * + " FROM study WHERE id = {id} AND " + condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
    
  def create(title : String, description : Option[String] = None)(implicit site : Site) : Study = {
    val args = Anorm.Args('title -> title, 'description -> description)
    val id = Audit.SQLon(AuditAction.add, "study", Anorm.insertArgs(args), "id")(args : _*).single(scalar[Id])(site.db)
    make(id, title, description)
  }
}

object Slot extends ContainerView[Slot]("slot") {
  private[models] val baseRow = Anorm.rowMap(Tuple2.apply[Id, String] _, col("id"), col("ident"))
  private[models] def makeStudy(study : Study)(id : Id, ident : String) = new Slot(id, study, ident)
  private[models] def baseMake(study : Study) = (makeStudy(study) _).tupled
  private[models] override val * = col("id", "ident")

  private[models] val row = 
    (Study.row ~ baseRow) map {
      case (study ~ slot) => baseMake(study)(slot)
    }
  private[models] override val src = "slot JOIN study ON slot.study = study.id"
  private[this] def rowStudy(study : Study) =
    baseRow map { baseMake(study) }

  def get(i : Id)(implicit site : Site) : Option[Slot] =
    SQL("SELECT " + * + ", " + Study.* + " FROM " + src + " WHERE slot.id = {id} AND " + condition).
      on('id -> i, 'identity -> site.identity.id).singleOpt(row)(site.db)
  private[models] def getIdent(study : Study, ident : String)(implicit db : Site.DB) : Option[Slot] =
    SQL("SELECT " + * + " FROM slot WHERE study = {study} AND ident = {ident}").
      on('study -> study.id, 'ident -> ident).singleOpt(rowStudy(study))
  private[models] def getStudy(study : Study)(implicit db : Site.DB) : Seq[Slot] =
    SQL("SELECT " + * + " FROM slot WHERE study = {study} ORDER BY ident").
      on('study -> study.id).list(rowStudy(study))
    
  def create(study : Study, ident : Option[String])(implicit site : Site) : Option[Slot] = {
    val stmt = "(study, ident) VALUES ({study}, " + (if (ident.isEmpty) "next_slot_ident({study})" else "{ident}") + ")"
    try {
      val (id ~ idnt) = Audit.SQLon(AuditAction.add, table, stmt, "id, ident")('study -> study.id, 'ident -> ident.getOrElse("")).
        single(SqlParser.get[Id]("id") ~ SqlParser.get[String]("ident"))(site.db)
      Some(new Slot(id, study, idnt))
    } catch {
      case e : java.sql.SQLException if e.getMessage.startsWith("ERROR: duplicate key value violates unique constraint \"slot_study_ident_key\"") => None
      case _ : java.sql.SQLIntegrityConstraintViolationException => None
    }
  }
}

