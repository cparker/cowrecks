import scala.io.Source._
import scala.xml._
import java.io._
import org.w3c.tidy._
import com.mongodb._
import java.text._
import java.util.Date

val m = new Mongo
val db = m.getDB("wrecks")
val col = db.getCollection("wrecks")

val df = new SimpleDateFormat("yyMMdd")



(0 to 1050 by 25).foreach { offset =>
  val rawHTML = 
    fromURL("http://www.aviationarchaeology.com/src/dbasta.asp?thestate=CO&Submit9=Go&offset=%s"
      .format(offset)
    )
    .getLines
    .drop(2)
    .mkString

  val t = new Tidy
  val baos = new ByteArrayOutputStream
  val ps = new PrintStream(baos)
  t.setXmlOut(true)
  t.parse(new StringReader(rawHTML),ps)
  val result = baos.toString("iso-8859-1")
  val parsed = XML.loadString(result)

  val rows = 
    ((parsed \\ "table")(5) \\ "td")
    .map{ x => ""+
      x.text.trim
      .replaceAll("""[\t\n\x0B\f\r]""","")
      .replaceAll("""\s+"""," ") + "" }.toList.grouped(12)
    .toList
    .drop(1) // drop the column headers

  case class Wreck(
    date:Date,
    aircraftType:String,
    serialNum:String,
    squad:String,
    group:String,
    homeBase:String,
    af:String,
    action:String,
    d:String,
    pilot:String,
    state:String,
    location:String
  )

  def fix(str:String) = { 

    val res = str
      .replaceAll("""[^\x00-\x7f]""","")
      .trim
    res
  }

  rows.foreach { row =>
    try {
    val w = Wreck(
      df.parse(fix(row(0))), 
      fix(row(1)), 
      fix(row(2)), 
      fix(row(3)), 
      fix(row(4)), 
      fix(row(5)), 
      fix(row(6)), 
      fix(row(7)), 
      fix (row(8)), 
      fix(row(9)), 
      fix(row(10)), 
      fix(row(11))
    )

    println(w)

    val dbo = new BasicDBObject
    dbo.put("date",w.date)
    dbo.put("aircraftType",w.aircraftType)
    dbo.put("serialNum",w.serialNum)
    dbo.put("squad",w.squad)
    dbo.put("group",w.group)
    dbo.put("homeBase",w.homeBase)
    dbo.put("af",w.af)
    dbo.put("action",w.action)
    dbo.put("d",w.d)
    dbo.put("pilot",w.pilot)
    dbo.put("state",w.state)
    dbo.put("location",w.location)

    col.insert(dbo)

  } catch {
    case e:Exception =>
      println("skipping.....")
  }

  }

  
  Thread.sleep(1000)
}

