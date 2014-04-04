import scala.Math._


//  39.47509 Lng: -105.61705

val lat1 = 39.47509
val lng1 = -105.61705

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

val distMi = 100.0 // miles?

val distK = distMi * KMPerMile

val angDist = distK / earthRadK
val ang = toRadians(toAngle("S"))

println("angDist " + angDist)

println("ang " + ang)

val lat1R = toRadians(lat1)
val lng1R = toRadians(lng1)

val lat2 = asin(sin(lat1R) * cos(angDist) + cos(lat1R) * sin(angDist) * cos(ang))
val lat2R = lat2

val lng2 = lng1R + atan2(sin(ang)*sin(angDist)*cos(lat1R), cos(angDist)-sin(lat1R)*sin(lat2R))

val lng2R = lng2

val lat2D = toDegrees(lat2R)
val lng2D = toDegrees(lng2R)

println("new point %s, %s".format(lat2D, lng2D))
