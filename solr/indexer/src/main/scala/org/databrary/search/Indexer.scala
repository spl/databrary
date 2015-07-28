package org.databrary.search

import java.io.{File, PrintWriter}

import org.joda.time.{DateTimeZone, Duration, DateTime}
import org.json4s.{JsonAST, NoTypeHints}
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.json4s.JsonDSL._
import org.postgresql.jdbc4.Jdbc4Array
import scala.collection.convert.wrapAll._
import scala.collection.convert.decorateAll._
import scala.collection.mutable


/**
 * Converts the databrary postgresql database into a JSON file that can be indexed by solr using our schema.xml.
 */
object Indexer {

  import scalikejdbc._


  implicit val formats = Serialization.formats(NoTypeHints)

  /*
  Case class definitions for working with the database. These let us intake information coming back from the DB
  as objects, which lets us easily create the JSON documents later.
   */

  def convertVolumeIntoJson(vol: Volume): Unit = {

  }

  def main(args: Array[String]) {
    // ad-hoc session provider on the REPL
    implicit val session = AutoSession

    // Params passed in from outside world
    val dbHost = if (args.length > 0) args(0) else "localhost"
    val dbPort = if (args.length > 1) args(1) else "5432"
    val dbName = if (args.length > 2) args(2) else "databrary"
    val dbUser = if (args.length > 3) args(3) else "jesse"
    val dbPw = if (args.length > 4) args(4) else ""

    // Database information.
    ConnectionPool.singleton(s"jdbc:postgresql://$dbHost:$dbPort/$dbName", dbUser, dbPw)



    object SQLParty extends SQLSyntaxSupport[SQLParty] {
      def apply(rs: WrappedResultSet): SQLParty = new SQLParty(
        rs.long("id"), rs.string("name"), rs.string("prename"), rs.string("affiliation"))
    }

    object SQLContainer extends SQLSyntaxSupport[SQLContainer] {
      def apply(rs: WrappedResultSet): SQLContainer = new SQLContainer(
        rs.long("id"), rs.long("volume"), rs.string("name"),
        if (rs.dateOpt("date").isDefined) Some(rs.date("date").toJodaDateTime) else None, rs.stringOpt("release")
      )
    }

    object SQLContainerText extends SQLSyntaxSupport[SQLContainerText] {
      def apply(rs: WrappedResultSet): SQLContainerText = new SQLContainerText(
        rs.long("id"), rs.longOpt("metric"), rs.stringOpt("txt")
      )
    }

    object SQLVolume extends SQLSyntaxSupport[SQLVolume] {
      def apply(rs: WrappedResultSet): SQLVolume = new SQLVolume(
      // TODO add volume owners here
        rs.long("id"), rs.string("name"), rs.string("body"), rs.string("alias"), rs.stringOpt("citation"), rs.intOpt("year"), rs.stringOpt("url"), rs.arrayOpt("owners"))
    }

    object SQLVolumeText extends SQLSyntaxSupport[SQLVolumeText] {
      def apply(rs: WrappedResultSet): SQLVolumeText = new SQLVolumeText(
        rs.long("volume"), rs.string("text"))
    }

    object SQLRecord extends SQLSyntaxSupport[SQLRecord] {
      def apply(rs: WrappedResultSet): SQLRecord = new SQLRecord(
        rs.long("record"), rs.long("container"), if (rs.dateOpt("measureDate").isDefined) Some(rs.date("measureDate").toJodaDateTime) else None,
        if (rs.dateOpt("measureDate").isDefined && rs.dateOpt("containerDate").isDefined)
          Some(new Duration(rs.date("measureDate").toJodaDateTime, rs.date("containerDate").toJodaDateTime))
        else None)
    }

    object SQLExcerpt extends SQLSyntaxSupport[SQLRecord] {
      def apply(rs: WrappedResultSet): SQLExcerpt = new SQLExcerpt(
        rs.long("asset"), rs.string("segment"), rs.stringOpt("release")
      )
    }

    object SQLMetric extends SQLSyntaxSupport[SQLMetric] {
      def apply(rs: WrappedResultSet): SQLMetric = new SQLMetric(
        rs.long("id"), rs.string("name"), rs.stringOpt("assumed"), rs.arrayOpt("options")
      )
    }

    object ResultsContainer extends SQLSyntaxSupport[Sentence] {
      def apply(rs: WrappedResultSet): ResultsContainer = new ResultsContainer(
        rs.long("volumeId"), rs.long("containerId"), rs.long("recordId"), rs.string("volumeName"), rs.string("body"), rs.string("alias"),
        rs.string("containerName"), rs.stringOpt("containerDate"), rs.stringOpt("datum_date"), rs.doubleOpt("datum_number"),
        rs.stringOpt("datum_text"), rs.stringOpt("segment")
      )
    }


    /*
    Extract all of the volumes from the DB
     */
    val sQLVolumes = sql"""
         SELECT id, name, body, alias, volume_citation.head AS citation, volume_citation.url AS url,
         volume_citation.year AS year, owners
         FROM volume
         LEFT JOIN volume_citation ON volume.id = volume_citation.volume
         LEFT JOIN volume_access ON volume.id = volume_access.volume
         LEFT JOIN volume_owners ON volume.id = volume_owners.volume
         WHERE volume_access.children > 'NONE' AND volume_access.party = -1 AND volume.id > 0
    """.map(x => SQLVolume(x)).list().apply().map(x => x.volumeId -> x).toMap


    /*
    Extract all of the parties from the DB
    */
    val sQLAllParties = sql"""
      SELECT id, name, prename, affiliation FROM party WHERE id > 0
    """.map(x => SQLParty(x)).list().apply().map(x => x.partyId -> x).toMap

    val sQLAuthorizedParties = sql"""
      SELECT party.id AS id, name, prename, affiliation FROM party JOIN authorize_view ON party.id = child AND parent = 0 WHERE id > 0
    """.map(x => SQLParty(x)).list().apply()

    val sQLInstitutionsParties = sql"""
      SELECT party.id AS id, name, prename, affiliation FROM party LEFT JOIN account ON party.id = account.id WHERE account.id IS NULL AND party.id > 0
    """.map(x => SQLParty(x)).list().apply()

    sQLAuthorizedParties.foreach(x => sQLAllParties(x.partyId).isAuthorized = true)
    sQLInstitutionsParties.foreach(x => sQLAllParties(x.partyId).isInstitution = true)

    val sQLParties = sQLAllParties.values

    //    partyFilter :: PartyFilter -> Identity -> BS.ByteString
    //    partyFilter ¡PartyFilter{..} ident = BS.concat
    //      [ withq partyFilterAccess (const " JOIN authorize_view ON party.id = child AND parent = 0")
    //    , " WHERE id > 0"
    //    , withq partyFilterQuery (\n -> " AND " <> queryVal <> " ILIKE " <> pgLiteralRep (wordPat n))
    //    , withq partyFilterAccess (\a -> " AND site = " <> pgSafeLiteral a)
    //    , withq partyFilterInstitution (\i -> if i then " AND account.id IS NULL" else " AND account.password IS NOT NULL")
    //    , withq partyFilterAuthorize (\a -> let i = pgSafeLiteral a in " AND party.id <> " <> i <> " AND id NOT IN (SELECT child FROM authorize WHERE parent = " <> i <> " UNION SELECT parent FROM authorize WHERE child = " <> i <> ")")
    //    , withq partyFilterVolume (\v -> " AND id NOT IN (SELECT party FROM volume_access WHERE volume = " <> pgSafeLiteral (volumeId v) <> ")")
    //    , " ORDER BY name, prename"
    //    ]
    //    where
    //    withq v f = maybe "" f v
    //    wordPat = intercalate "%" . ("":) . (++[""]) . words
    //    queryVal
    //    | showEmail ident = "(COALESCE(prename || ' ', '') || name || COALESCE(' ' || email, ''))"
    //    | otherwise = "(COALESCE(prename || ' ', '') || name)"

    /*
    Get all of the containers from the DB and create a container->volume lookup table
     */
    val sQLContainers = sql"""
        SELECT id, container.volume AS volume, name, date, release FROM container
        LEFT JOIN slot_release ON id = slot_release.container
        LEFT JOIN volume_access ON container.volume = volume_access.volume
        WHERE volume_access.children > 'NONE' AND volume_access.party = -1 AND container.volume > 0
    """.map(x => SQLContainer(x)).list().apply().map(x => x.containerId -> x).toMap

    // This will get all of the text for the containers as well as that texts' metric
    val sQLContainersText = sql"""
           SELECT slot_record.container AS id, datum AS txt, measure_text.metric AS metric
           FROM slot_record, measure_text
           LEFT JOIN metric ON metric.id = measure_text.metric
           WHERE slot_record.record = measure_text.record AND metric.release >= 'EXCERPTS'
    """.map(x => SQLContainerText(x)).list().apply()

    val sQLMetrics = sql"""
      SELECT id, name, assumed, options FROM metric
    """.map(x => SQLMetric(x)).list().apply().map(x => x.metricId -> x).toMap

    sQLContainersText.foreach{ x =>
      val c = sQLContainers.getOrElse(x.containerId, null)
      if(c != null && x.metric.isDefined && x.text.isDefined) {
        val measureName = sQLMetrics(x.metric.get).metricName
        measureName match {
          case "race" => if (c.race.isDefined) c.race = Some(c.race.get + " " +  x.text.get) else c.race = x.text
          case "ethnicity" => if (c.ethnicity.isDefined) c.ethnicity = Some(c.ethnicity.get + " " +  x.text.get) else c.ethnicity = x.text
          case "gender" => if (c.gender.isDefined) c.gender = Some(c.gender.get + " " +  x.text.get) else c.gender = x.text
          case "state" => if (c.state.isDefined) c.state = Some(c.state.get + " " + x.text.get) else c.state = x.text
          case "setting" => if (c.setting.isDefined) c.setting = Some(c.setting.get + " " + x.text.get) else c.setting = x.text
          case "language" => if (c.language.isDefined) c.language = Some(c.language.get + " " + x.text.get) else c.language = x.text
          case "country" => if (c.country.isDefined) c.country = Some(c.country.get + " " + x.text.get) else c.country = x.text
          case _ => if (c.text.isDefined) c.text = Some(c.text.get + " " + x.text.get) else c.text = x.text
        }
      }
    }

//    val sQLContainers = sql"""
//       SELECT id, container.volume AS volume, name, date, release, string_agg(datum, ' ') AS txt
//       FROM container
//       LEFT JOIN slot_release ON id = slot_release.container
//       LEFT JOIN volume_access ON container.volume = volume_access.volume
//       LEFT JOIN slot_record ON id = slot_record.container
//       LEFT JOIN measure_text ON slot_record.record = measure_text.record
//       WHERE volume_access.children > 'NONE' AND volume_access.party = -1 AND container.volume > 0
//       GROUP BY container.id, container.volume, name, date, release
//    """.map(x => SQLContainer(x)).list().apply().map(x => x.containerId -> x).toMap

    val sQLContainerVolumeLookup = sQLContainers.values.map(x => x.containerId -> sQLVolumes(x.volumeId))
      .toMap.withDefaultValue(new SQLVolume(volumeId = -1))

    val containerIdList = sQLContainers.values.map(x => x.containerId)
    val volumeIdList = sQLVolumes.values.map(x => x.volumeId)

    /*
    Set whether or not this volume actually contains sessions... going to make this public sessions
    */
    sQLContainers.values.map {
      x =>
        if (x.release.getOrElse("") == "EXCERPTS")
          sQLVolumes(x.volumeId).hasSessions = true
    }


    /*
    These are methods that extract each type of segment into scala objects.
     */
    object SQLSegmentTag extends SQLSyntaxSupport[SQLSegmentTag] {
      // We need a lookup function for getting the other information that we need out of this thing
      def apply(rs: WrappedResultSet): SQLSegmentTag = new SQLSegmentTag(
        sQLContainerVolumeLookup(rs.long("container").toInt).volumeId, rs.long("container"), rs.string("segment"), rs.stringOpt("tag")
      )
    }

    /*
      Ditto for segment records
     */
    object SQLSegmentRecord extends SQLSyntaxSupport[SQLSegmentTag] {
      def apply(rs: WrappedResultSet): SQLSegmentRecord = new SQLSegmentRecord(
        sQLContainerVolumeLookup(rs.long("container").toInt).volumeId, rs.long("container"), rs.string("segment"),
        rs.long("record"), rs.string("metric"), rs.stringOpt("datum")
      )
    }

    object SQLSegmentAsset extends SQLSyntaxSupport[SQLSegmentTag] {
      def apply(rs: WrappedResultSet): SQLSegmentAsset = new SQLSegmentAsset(
        rs.long("volume"), rs.long("container"), rs.string("segment"), rs.long("asset"), rs.string("name"), rs.string("duration"), rs.stringOpt("excerpt").isDefined
      )
    }

    object SQLSegmentRelease extends SQLSyntaxSupport[SQLSegmentTag] {
      def apply(rs: WrappedResultSet): SQLSegmentRelease = new SQLSegmentRelease(
        sQLContainerVolumeLookup(rs.long("container").toInt).volumeId, rs.long("container"), rs.string("segment"), rs.string("release")
      )
    }


    val sQLRecords = sql"""
        SELECT container.id AS container, record.id AS record, container.date AS containerDate, measure_date.datum AS measureDate FROM container, slot_record, record, measure_date
              WHERE container.id = slot_record.container AND record.id = slot_record.record
              AND measure_date.record = record.id
              """.map(x => SQLRecord(x)).list().apply().filter(x => sQLContainerVolumeLookup(x.containerId).volumeId > 0)
      .groupBy(x => x.containerId).map(x => x._1 -> x._2)
    //
    //    // We now want to go a step further and get all of the segments, i.e., all of the records/measures

    val sQLSegmentTags = sql"""
      SELECT container, segment, tag.name AS tag FROM tag_use, tag WHERE tag = id
      """.map(x => SQLSegmentTag(x)).list().apply().filter(x => x.volumeId > 0)
    sQLSegmentTags.groupBy(x => x.volumeId).map(x => x._1 -> x._2.map(y => y.tags.getOrElse(""))).map(x => sQLVolumes(x._1).tags = x._2)

    val sQLSegmentKeywords = sql"""
      SELECT container, segment, tag.name AS tag FROM keyword_use, tag WHERE tag = id
      """.map(x => SQLSegmentTag(x)).list().apply().filter(x => x.volumeId > 0)
    sQLSegmentKeywords.groupBy(x => x.volumeId).map(x => x._1 -> x._2.map(y => y.tags.getOrElse(""))).map(x => sQLVolumes(x._1).keywords = x._2)

    val sQLVolumeText = sql"""
      SELECT volume, text FROM volume_text
      """.map(x => SQLVolumeText(x)).list().apply().groupBy(x => x.volumeId).map(x => x._1 -> x._2.map(y => y.Text).mkString(" "))

    sQLVolumeText.map(x => if(sQLVolumes.keySet contains x._1) sQLVolumes(x._1).allText = x._2)

    // This will get all of the public measures
    val sQLSegmentRecords = sql"""
         SELECT container, segment, slot_record.record AS record, metric.name AS metric, datum
         FROM slot_record, measure as measure_all, metric
         WHERE measure_all.record = slot_record.record AND measure_all.metric = metric.id AND metric.release >= 'EXCERPTS'
         """.map(x => SQLSegmentRecord(x)).list().apply().filter(x => x.volumeId > 0)


    sql"""
         SELECT container, segment, slot_record.record AS record, metric.name AS metric, datum
         FROM slot_record, measure as measure_all, metric
         WHERE measure_all.record = slot_record.record AND measure_all.metric = metric.id AND metric.name = 'birthdate'
       """.map(x => SQLSegmentRecord(x)).list().apply().map { x =>
      if (sQLContainers.contains(x.containerId)) {
        sQLContainers(x.containerId).age =
          if (sQLContainers(x.containerId).date.isDefined && x.datum.isDefined && x.metric == "birthdate")
            Some(new Duration(new DateTime(x.datum.get), sQLContainers(x.containerId).date.get).getStandardDays)
          else None
      }
    }


    val sQLSegmentAssets = sql"""
        SELECT container, slot_asset.segment, asset.id AS asset, asset.name AS name, asset.duration AS duration, asset.release AS release, volume, excerpt.segment AS excerpt
                  FROM slot_asset, asset LEFT JOIN excerpt ON asset.id = excerpt.asset
                  WHERE slot_asset.asset = asset.id
                  """.map(x => SQLSegmentAsset(x)).list().apply().filter(x => x.volumeId > 0)

    val sQLSegmentReleases = sql"""
         SELECT container, segment, release FROM slot_release
         """.map(x => SQLSegmentRelease(x)).list().apply().filter(x => x.volumeId > 0)

    /*
      Go through and mark containers and volumes that have excerpts as having them
    */
    sQLSegmentAssets.map {
      x =>
        if (x.isExcerpt && sQLContainers.contains(x.containerId)) {
          val c = sQLContainers(x.containerId.toInt)
          c.hasExcerpt = true // Gross :(
          sQLContainerVolumeLookup(x.containerId.toInt).hasExcerpt = true
        }
    }

    //    sQLSegmentRecords.map(x => println(x.volumeId, x.containerId, x.record, x.segment, x.metric, x.date, x.num, x.text))


    /*
      Collate all of the documents into JSONDocuments and create one big list to write them all out in.
     */
    val jsonDocuments = sQLVolumes.map(x => createVolumeDocument(x._2)) ++
      sQLContainers.map(x => createContainerDocument(x._2)) ++
      sQLRecords.flatMap(x => x._2.map(y => createRecordDocument(y))) ++
      sQLSegmentRecords.map(x => createSegmentRecordDocument(x)) ++
      sQLSegmentAssets.map(x => createSegmentAssetDocument(x)) ++
      sQLSegmentTags.map(x => createSegmentTagDocument(x)) ++
      sQLParties.map(x => createPartyDocument(x))


    /*
      Finally, write the JSON to disk as a giant file.
     */
    val jsonVol = write(jsonDocuments.toSeq)

    val pw = new PrintWriter(new File("databrary.json"))
    pw.write(jsonVol)
    pw.close()
    println("t")
  }

  def createVolumeDocument(vol: SQLVolume) = {
    println(vol.owners)
    val (ownerIds, ownerNames) = if(vol.owners.isDefined) {
      val ids = Some(vol.owners.get.getArray().asInstanceOf[Array[String]].map(x => x.split(":")(0).toInt).toSeq)
      val names = Some(vol.owners.get.getArray().asInstanceOf[Array[String]].map(x => x.split(":")(1)).toSeq)
      (ids, names)
    } else {
      (None, None)
    }
    println(ownerIds)
    println(ownerNames)
    new JsonDocument(content_type = ContentTypes.VOLUME, volume_id_i = Some(vol.volumeId.toInt),
      abs_t = Some(vol.abs), title_t = Some(vol.title), alias_s = Some(vol.alias), citation_t = vol.cite,
      citation_url_s = vol.citeUrl, citation_year_i = vol.citeYear, volume_has_excerpt_b = Some(vol.hasExcerpt),
      volume_has_sessions_b = Some(vol.hasSessions),
      volume_keywords_ss = if(vol.keywords != null) Some(("boost" -> 3.0 ) ~ ("value" -> Option(vol.keywords))) else None,
      volume_tags_ss = if(vol.tags != null) Some(("boost" -> 2.0 ) ~ ("value" -> Option(vol.tags))) else None,
      volume_all_text_t = Option(vol.allText),
      volume_owner_ids_is = ownerIds,
      volume_owner_names_ss = ownerNames
    )
  }

  def createContainerDocument(container: SQLContainer) = {
    new JsonDocument(content_type = ContentTypes.CONTAINER, container_volume_id_i = Some(container.volumeId.toInt),
      container_id_i = Some(container.containerId.toInt),
      container_date_tdt = None, // This should never be set for privacy reasons
      container_name_t = Some(container.name), container_age_td = container.age,
      container_has_excerpt_b = Some(container.hasExcerpt),
      container_text_t = container.text,
      container_country_s = container.country,
      container_gender_s = container.gender,
      container_language_s = container.language,
      container_state_s = container.state,
      container_ethnicity_s = container.ethnicity,
      container_race_s = container.race,
      container_setting_s = container.setting
    )
  }

  def createRecordDocument(record: SQLRecord) = {
    new JsonDocument(content_type = ContentTypes.RECORD, record_container_i = Some(record.containerId.toInt),
      record_id_i = Some(record.recordId.toInt),
      record_date_tdt = None
    )
  }

  def createSegmentTagDocument(segment: SQLSegmentTag) = {
    new JsonDocument(content_type = ContentTypes.SEGMENT_TAG, segment_volume_id_i = Some(segment.volumeId.toInt),
      segment_container_id_i = Some(segment.containerId.toInt), segment_tags_ss = Some(segment.tags.get.split(","))
    )
  }

  def createSegmentAssetDocument(segment: SQLSegmentAsset) = {
    new JsonDocument(content_type = ContentTypes.SEGMENT_ASSET, segment_volume_id_i = Some(segment.volumeId.toInt),
      segment_container_id_i = Some(segment.containerId.toInt), segment_asset_i = Some(segment.asset.toInt),
      segment_asset_name_s = Some(segment.assetName)
    )
  }

  def createSegmentRecordDocument(segment: SQLSegmentRecord) = {
    new JsonDocument(content_type = ContentTypes.SEGMENT_RECORD, segment_volume_id_i = Some(segment.volumeId.toInt),
      segment_container_id_i = Some(segment.containerId.toInt), segment_record_id_i = Some(segment.record.toInt),
      record_text_t = segment.datum, record_metric = Some(segment.metric)
    )
  }

  def createPartyDocument(party: SQLParty) = {
    new JsonDocument(content_type = ContentTypes.PARTY, party_affiliation_s = Some(party.affiliation),
      party_id_i = Some(party.partyId.toInt),
      party_name_s = Some(party.name), party_pre_name_s = Some(party.preName),
      party_is_institution_b = Some(party.isInstitution),
      party_is_authorized_b = Some(party.isAuthorized)
    )
  }

  case class Sentence(volId: Long, text: Option[String])

  case class SQLVolume(volumeId: Long, title: String = "", abs: String = "", alias: String = "", cite: Option[String] = None,
                       citeYear: Option[Int] = None, citeUrl: Option[String] = None, owners: Option[java.sql.Array] = None,
                       var hasExcerpt: Boolean = false,
                       var hasSessions: Boolean = false,
                       var tags: Seq[String] = null,
                       var keywords: Seq[String] = null,
                       var allText: String = "")

  case class Volume(volumeId: Long, title: String, abs: String, alias: String, containers: Seq[Container], var hasExcerpt: Boolean = false, var hasSessions: Boolean = false)

  case class SQLContainer(containerId: Long, volumeId: Long, name: String, date: Option[DateTime] = None, release: Option[String] = None,
                          var text: Option[String] = None, var age: Option[Double] = None, var hasExcerpt: Boolean = false,
                          var state: Option[String] = None, var ethnicity: Option[String] = None,
                          var gender: Option[String] = None, var language: Option[String] = None, var race: Option[String] = None,
                          var setting: Option[String] = None, var country: Option[String] = None)

  case class SQLContainerText(containerId: Long, metric: Option[Long], text: Option[String])

  case class Container(containerId: Long, volumeId: Long, name: String, date: Option[DateTime], release: Option[String], records: Seq[Record], hasExcerpt: Boolean)

  case class SQLRecord(recordId: Long, containerId: Long, date: Option[DateTime], age: Option[Duration])

  case class Record(recordId: Long, containerId: Long, date: Option[DateTime], age: Option[Duration])

  case class SQLExcerpt(assetId: Long, segment: String, release: Option[String])

  case class Excerpt(assetId: Long, segment: String, release: Option[String])

  case class SQLVolumeText(volumeId: Long, Text: String)

  case class SQLSegmentTag(volumeId: Long, containerId: Long, segment: String, tags: Option[String])

  case class SQLSegmentRecord(volumeId: Long, containerId: Long, segment: String, record: Long, metric: String, datum: Option[String])

  case class SQLSegmentRelease(volumeId: Long, containerId: Long, segment: String, release: String)

  /*
    Methods for converting case classes into JSONDocuments
   */

  /* TODO
  We need to be able to easily extract excerpts. There are a few ways to do this:
  1 create excerpt objects that we can query
  2 attach excerpt info to each asset, set in container appropriately

  We probably want to just mark a volume as having an excerpt and then let the system handle it rather than try
  to store it ourselves
   */
  case class SQLSegmentAsset(volumeId: Long, containerId: Long, segment: String, asset: Long, assetName: String, duration: String, isExcerpt: Boolean)

  case class SQLMetric(metricId: Long, metricName: String, assumed: Option[String], options: Option[java.sql.Array])

  case class SQLParty(partyId: Long, name: String, preName: String, affiliation: String, var isAuthorized: Boolean = false, var isInstitution: Boolean = false)

  case class Party(partyId: Long, name: Option[String], preName: Option[String], affiliation: Option[String])

  case class Segment(containerId: Long, recordId: Long, interval: Duration)

  case class SearchContainer(id: Long, volumeId: Long, containerId: Long, recordId: Long,
                             title: String, abs: String, alias: String,
                             containerName: String, containerDate: Option[String],
                             recordDate: Option[String], age: Option[String])

  //  volume.id, container.id, record.id, volume.name, volume.body, volume.alias, container.name, container.date
  //  measure_all.datum_date, measure_all.datum_number, measure_all.datum_text, slot_record.segment
  case class ResultsContainer(volumeId: Long, containerId: Long, recordId: Long,
                              title: String, abs: String, alias: String,
                              containerName: String, containerDate: Option[String],
                              recordDate: Option[String], measure_number: Option[Double],
                              measure_text: Option[String], segment: Option[String])

  case class JsonContainer(volumeId: Long, containerId: Long, recordId: Long, segmentId: Long,
                           title: String, abs: String,
                           containerName: String, measure_number: Option[Double],
                           measure_text: Option[String], segment: Option[String], age: Option[String])

  // Named in an ugly way for automatic identification by Solr's naming schema
  case class JsonDocument(content_type: String,
                          volume_id_i: Option[Int] = None,
                          volume_has_excerpt_b: Option[Boolean] = None,
                          volume_has_sessions_b: Option[Boolean] = None,
                          volume_keywords_ss: Option[JsonAST.JObject] = None,
                          volume_tags_ss: Option[JsonAST.JObject] = None,
                          volume_all_text_t: Option[String] = None,
                          volume_owner_names_ss: Option[Seq[String]] = None,
                          volume_owner_ids_is: Option[Seq[Int]] = None,
                          alias_s: Option[String] = None,
                          title_t: Option[String] = None,
                          abs_t: Option[String] = None,
                          citation_t: Option[String] = None,
                          citation_year_i: Option[Int] = None,
                          citation_url_s: Option[String] = None,
                          container_id_i: Option[Int] = None,
                          container_volume_id_i: Option[Int] = None,
                          container_date_tdt: Option[String] = None,
                          container_name_t: Option[String] = None,
                          container_age_td: Option[Double] = None,
                          container_has_excerpt_b: Option[Boolean] = None,
                          container_keywords_ss: Option[Seq[String]] = None,
                          container_text_t: Option[String] = None,
                          container_state_s: Option[String] = None,
                          container_ethnicity_s: Option[String] = None,
                          container_gender_s: Option[String] = None,
                          container_language_s: Option[String] = None,
                          container_race_s: Option[String] = None,
                          container_setting_s: Option[String] = None,
                          container_country_s: Option[String] = None,
                          record_id_i: Option[Int] = None, record_volume_id_i: Option[Int] = None,
                          record_container_i: Option[Int] = None, record_date_tdt: Option[String] = None,
                          record_text_t: Option[String] = None,
                          record_num_d: Option[Double] = None,
                          record_metric: Option[String] = None,
                          segment_volume_id_i: Option[Int] = None, segment_record_id_i: Option[Int] = None,
                          segment_container_id_i: Option[Int] = None,
                          segment_start_tl: Option[Long] = None, segment_end_tl: Option[Long] = None,
                          segment_length_tl: Option[Long] = None,
                          segment_tags_ss: Option[Seq[String]] = None,
                          segment_asset_i: Option[Int] = None,
                          segment_asset_name_s: Option[String] = None,
                          segment_release_s: Option[String] = None,
                          party_pre_name_s: Option[String] = None,
                          party_name_s: Option[String] = None,
                          party_affiliation_s: Option[String] = None,
                          party_id_i: Option[Int] = None,
                          party_is_institution_b: Option[Boolean] = None,
                          party_is_authorized_b: Option[Boolean] = None
                           )

  /*
  Names of the different types of content we have inside of Solr
   */
  object ContentTypes {
    val VOLUME = "volume"
    val CONTAINER = "container"
    val RECORD = "record"
    val SEGMENT_TAG = "segment_tag"
    val SEGMENT_RECORD = "segment_record"
    val SEGMENT_ASSET = "segment_asset"
    val SEGMENT_RELEASE = "segment_release"
    val PARTY = "party"
  }

}
