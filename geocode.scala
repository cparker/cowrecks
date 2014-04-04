import scala.io.Source._
import scala.xml._
import java.io._
import org.w3c.tidy._
import com.mongodb._
import java.text._
import java.util.Date
import java.net.URLEncoder
import net.liftweb.json._
import scala.collection.JavaConversions._

val m = new Mongo
val db = m.getDB("wrecks")
val col = db.getCollection("wrecks")

// URL for google maps geocoding web service, using 'viewport biasing' where the bounds == state of CO
val geourl = "http://maps.googleapis.com/maps/api/geocode/json?address=%s&sensor=false&bounds=36.2900318153113,-111.66266420312502|42.3030378984948,-99.80841615625002"

val locParseRX = """(?i)^(?:(\d+\.\d+)|(\d+)\s+\d+\/\d+|\d+\/\d+|(\d+))\s*?(?:mi|m)\s*?([nesw]{1,3})\s*(?:of)?\s*(.*?)$""".r

val query = BasicDBObjectBuilder.start
  .add("locDetail", BasicDBObjectBuilder.start
    .add("$exists" , false)
    .get
  ).get

  case class LocParsed(
    cityState:String,
    dirOffset:String,
    offsetDist:Float
  )

  case class LocDetail( lat:Long, lng:Long)



col.find(query).iterator.toIterable.foreach { rec =>
  try {
    val locStr = rec.get("location") match { case s:String => s; case _ => "" }
    println("locStr " + locStr)

    val locFiltered = locStr
      .replaceAll("""(?i)^(.*?\s*)AAB(\s*.*$)""","$1Air Force Base $2") // AAB  => Army Air Base
      .replaceAll("""(?i)^(.*?\s*)AAF(\s*.*$)""","$1Air Force Base $2") // AAF  => Army Air Field
      .replaceAll("""(?i)^(.*?\s*)Co\s*Sprgs(\s*.*$)""","$1Colorado Springs $2") // Sprgs => Springs
      .replaceAll("""(?i)^(.*?\s*)Aux Fld(\s*.*$)""","$1Air Force Base $2") // AAB  => Army Air Base
      .replaceAll("""(?i)^(.*?\s*)Aux\. Fld(\s*.*$)""","$1Air Force Base $2") // AAB  => Army Air Base
      .replaceAll("""(?i)^(.*?\s*)Aux\. Field(\s*.*$)""","$1Air Force Base $2") // AAB  => Army Air Base

    println("location filtered " + locFiltered)


    locParseRX.findFirstMatchIn(locFiltered) match {

      // we were able to parse out the NW SE parts
      case Some(m) =>
        val parts = m.subgroups.flatMap{ Option(_) }

        val locParsed = 
          if (parts.size >= 3)
            LocParsed(cityState = parts(2), dirOffset = parts(1), offsetDist = parts(0).toFloat)
          else
            LocParsed(cityState = parts(1), dirOffset = parts(0), offsetDist = 0)
          
        println("loc parsed " + locParsed)
        val locParsedUpdateDBO = locParsedUpdate(locParsed)
        println("updating record with %s ".format(locParsedUpdateDBO))
        col.update(rec, locParsedUpdateDBO,true,false)

        println("querying google...")
        val result = fromURL(geourl.format(URLEncoder.encode(locParsed.cityState))).mkString
        googleResultToLL(parse(result)) match {
          case Some((lat,lng)) =>
            val (offsetLat, offsetLng) = 
              calculateOffset(lat,lng,locParsed.dirOffset, locParsed.offsetDist)

            val locDetailUpdateDBO = locDetailUpdate(offsetLat,offsetLng)
            println("updating record with %s".format(locDetailUpdateDBO))
            col.update(rec,locDetailUpdateDBO,true,false)


          case None =>
            println("no lat lng results")
            println("result")
        }

      // we weren't able to parse out the parts, just send the whole thing to google
      case None =>
        println("querying google...")
        val result = fromURL(geourl.format(URLEncoder.encode(locFiltered))).mkString
        googleResultToLL(parse(result)) match {
          case Some((lat,lng)) =>
            val locDetailUpdateDBO = locDetailUpdate(lat,lng)
            println("updating record with %s".format(locDetailUpdateDBO))
            col.update(rec,locDetailUpdateDBO,true,false)

          case None =>
            println("no lat lng results")
            println(result)
        }
    } // end match
  } // end try
  catch {
    case e:Exception =>
      println("caught " + e)
  }

  println("\n-------------------------------\n")
  Thread.sleep(1000)
} // end foreach



def calculateOffset(lat1:Double, lng1:Double, dir:String, distMiles:Float):(Double,Double) = {
  import scala.math._
  
  val earthRadK = 6371.8 // radius of the earth in KM

  val KMPerMile = 1.609344

  def toAngle(dir:String) = dir.toLowerCase match {
    case "n" =>   0.0
    case "nne" => 22.5
    case "ne" =>  45.0
    case "ene" => 67.5
    case "e" =>   90.0
    case "ese" => 112.5
    case "se" =>  135.0
    case "sse" => 157.5
    case "s" =>   180.0
    case "ssw" => 202.5
    case "sw" =>  225.0
    case "wsw" => 247.5
    case "w" =>   270.0
    case "wnw" => 292.5
    case "nw" =>  315.0
    case _ =>     337.5
  }

  val distK = distMiles * KMPerMile

  val angDist = distK / earthRadK
  val ang = toRadians(toAngle(dir))

  val lat1R = toRadians(lat1)
  val lng1R = toRadians(lng1)

  val lat2 = asin(sin(lat1R) * cos(angDist) + cos(lat1R) * sin(angDist) * cos(ang))
  val lat2R = lat2

  val lng2 = lng1R + atan2(sin(ang)*sin(angDist)*cos(lat1R), cos(angDist)-sin(lat1R)*sin(lat2R))

  val lng2R = lng2

  val lat2D = toDegrees(lat2R)
  val lng2D = toDegrees(lng2R)

  (lat2D,lng2D)

}


def googleResultToLL(result:JValue):Option[(Double,Double)] = {
  result \ "results" match {
    case JArray(a) if (a.size > 0) =>
      a.find{ jo => jo \ "geometry" \ "location" != JNothing } match {
        case Some(goodResult) =>
          (goodResult \ "geometry" \ "location" \ "lat",
          goodResult \ "geometry" \ "location" \ "lng") match {
            case (JDouble(latD), JDouble(lngD)) =>
              Some (latD,lngD)
            case _ => None
          }
        case None => None
      }

    case jo  => 
      (jo \ "geometry" \ "location" \ "lat",
      jo \ "geometry" \ "location" \ "lng") match {
        case (JDouble(latD), JDouble(lngD)) =>
          Some (latD,lngD)
        case _ => None
      }
  }
}


def locDetailUpdate(lat:Double,lng:Double):DBObject = {
  BasicDBObjectBuilder.start
  .add("$set", BasicDBObjectBuilder.start
    .add("locDetail",
      BasicDBObjectBuilder.start
        .add("lat", lat)
        .add("lng", lng)
        .get
      ).get
    ).get
}

def locParsedUpdate(locParsed:LocParsed): DBObject = {
        BasicDBObjectBuilder.start
        .add("$set", BasicDBObjectBuilder.start
          .add("locParsed", 
            BasicDBObjectBuilder.start
              .add("cityState", locParsed.cityState)
              .add("dirOffset", locParsed.dirOffset)
              .add("offsetDist", locParsed.offsetDist)
              .get
          ).get
        ).get

}
