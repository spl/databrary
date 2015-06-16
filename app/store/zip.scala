package store

import scala.collection.mutable
import scala.concurrent.Future
import macros.async._
import play.api.libs.json.JsValue
import play.api.libs.iteratee._
import org.databrary.iteratee.ZipFile
import macros._
import site._
import models._

object Zip {
  implicit val executionContext = context.foreground

  private def comment(obj : SiteObject) : String =
    Site.url(obj.pageURL)

  private def enum[A](a : A, e2 : Future[Enumerator[A]]) : Enumerator[A] =
    Enumerator(a) >>> Enumerator.flatten(e2)


  def slotAssetList(slot : Slot, prefix : String = "") : Future[Seq[(String, JsValue)]] =
    slotName(slot).flatMap { sname =>
    slot.assets.map { assets =>
      val names = mutable.Set.empty[String]
      Seq(assets.flatMap(cast[SlotFileAsset](_).filter(_.checkPermission(Permission.VIEW))).map { sa =>
        val base = sa.asset.name.getOrElse(sa.asset.format.name)
        val ext = sa.format.extension.fold("")("." + _)
        var name = base + ext
        var i = 1
        while (!names.add(name)) {
          i += 1
          name = base + i + ext
        }
        (prefix + sname + "/" + name , sa.json.js)
      } : _*)
    }}

  private def slotAssets(slot : Slot, prefix : String) : Future[Enumerator[ZipFile.StreamEntry]] =
    slot.assets.map { assets =>
      val names = mutable.Set.empty[String]
      Enumerator(assets.flatMap(cast[SlotFileAsset](_).filter(_.checkPermission(Permission.VIEW))).map { sa =>
        val base = sa.asset.name.getOrElse(sa.asset.format.name)
        val ext = sa.format.extension.fold("")("." + _)
        var name = base + ext
        var i = 1
        while (!names.add(name)) {
          i += 1
          name = base + i + ext
        }
        new ZipFile.DeflatedStreamEntry(prefix + name,
          Enumerator.flatten(Asset.read(sa)),
          Asset.timestamp(sa),
          comment = comment(sa))
      } : _*)
    }



  private def slotName(slot : Slot) : Future[String] =
    slot.fileName.map(_ + "-" + slot.containerId)

  private def zip(obj : SiteObject)(entries : Future[Enumerator[ZipFile.StreamEntry]]) : Enumerator[Array[Byte]] =
    Enumerator.flatten(entries) &>
      ZipFile.flatZip(comment = comment(obj), level = java.util.zip.Deflater.BEST_SPEED)

  private def slotEntries(slot : Slot, prefix : String) : Future[(String, Enumerator[ZipFile.StreamEntry])] =
    slotName(slot).map { name =>
      name ->
      (if (name.nonEmpty)
        enum(new ZipFile.DirEntry(prefix + name, comment = comment(slot)),
          slotAssets(slot, prefix + name + "/"))
      else Enumerator.flatten(slotAssets(slot, prefix)))
    }

  def slot(slot : Slot) : Enumerator[Array[Byte]] = zip(slot) {
    slot.volume.fileName.map { vname =>
      enum(new ZipFile.DirEntry(vname, comment = comment(slot.volume)),
        slotEntries(slot, vname + "/").map(_._2))
    }
  }

  def volumeAssetList (vol : Volume): Future[Seq[(String, JsValue)]]  = {
    vol.fileName.flatMap { vname =>
      vol.containers.flatMap { slots =>
        slots.flatMapAsync(slotAssetList(_, vname + "/"))
      }
    }
  }
    
  def volume(vol : Volume) = zip(vol) {
    vol.fileName.map { vname =>
      enum(new ZipFile.DirEntry(vname, comment = comment(vol)),
        vol.containers.map { slots =>
          val prefix = vname + "/"
          Enumerator(slots : _*) &> Enumeratee.mapFlatten[Container] { slot =>
            Enumerator.flatten {
              for {
                (name, ents) <- slotEntries(slot, prefix)
              } yield {
                ents
              }
            }
          }
        }
      )
    }
  }
}
