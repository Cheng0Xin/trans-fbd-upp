package mcfsm

import scala.xml.XML
import scala.xml.Node
import scala.xml.NodeSeq
import mcfsm.FBD.FBInstance
import mcfsm.FBD.Connection

object ParserException {
  class ParseValueNotFoundException extends RuntimeException
  class FBInstanceNotFoundException extends RuntimeException
}

object FBD {
  case class FBType(name: String, inputs: List[String],
    outputs: List[String], maps: List[String] => String)
  case class FBInstance(name: String, fbType: FBType, order: Int,
    values: List[String])
  case class Connection(srcIns: FBInstance, srcPort: String,
    dstIns: FBInstance, dstPort: String)

  val MC_MoveAbsolute = FBType("MC_MoveAbsolute",
    List.empty, List.empty, x => x.toString)
  val MC_MoveVelocity = FBType("MC_MoveVelocity",
    List.empty, List.empty, x => x.toString)
  val AND = FBType("AND", List.empty,
    List.empty, x => x.toString)
}


object Parser {
  def loadApplication(filename: String): NodeSeq = {
    val data = XML.load(filename)
    data \ "Application"
  }

  def getSubAppNetwork(ns: NodeSeq): NodeSeq = {
    ns \ "SubAppNetwork"
  }

  def convertSubAppNetwork(n: Node) = {

    def convertFB(x: Node): FBInstance = {
      // Get name of instance from XML
      val fbName = x.attribute("Name") match {
        case Some(value) => value.toString 
        case None => throw new ParserException.ParseValueNotFoundException
      }

      // Get order of instance
      val fbOrder = x.attribute("Order") match {
        case Some(value) => value.toString.trim.toInt
        case None => 0
      }

      // Get type of instance
      val fbType = x.attribute("Type") match {
        case Some(value) => {
          value.toString match {
            case "MC_MoveAbsolute" => FBD.MC_MoveAbsolute
            case "MC_MoveVelocity" => FBD.MC_MoveVelocity
            case "AND" => FBD.AND
            case _ => throw new ParserException.ParseValueNotFoundException
          }
        }
        case None => throw new ParserException.ParseValueNotFoundException
      }

      // Get value list
      val fbValList = x \ "Parameter"
      var fbValList_Res = List.empty[String]
      if (fbValList != null) {
        fbValList_Res = fbValList.map(valx => {
          val valxVal = valx.attribute("Value") match {
            case Some(value) => value
            case None => throw new ParserException.ParseValueNotFoundException
          }
          val valxName = valx.attribute("Name") match {
            case Some(value) => value
            case None => throw new ParserException.ParseValueNotFoundException
          }
          f"$valxName=$valxVal"
        }).toList
      }

      FBInstance(fbName, fbType, fbOrder, fbValList_Res)
    }

    def convertConnection(x: Node, insl: List[FBInstance]): Connection = {
      val getNamePortPair: String => (String, String) = tx => {
        x.attribute(tx) match {
          case None => throw new ParserException.ParseValueNotFoundException
          case Some(value) => {
            val res = value.toString.trim.split("\\.")
            
            (res(0), res(1))
          }
        }
      }
      val (src, srcPort) = getNamePortPair("Source")
      val (dst, dstPort) = getNamePortPair("Destination") 

      Connection(findOne(src, insl), srcPort, findOne(dst, insl), dstPort)
    }

    def findOne(target: String, l: List[FBInstance]): FBInstance = {
      val resList = l.filter(x => x.name == target)
      if (resList.length != 1) {
        throw new ParserException.FBInstanceNotFoundException
      } else {
        resList.head
      }
    }

    val fbis = n \ "FB"
    val cnns = n \ "DataConnections" \ "Connection"

    val fbinsList = fbis.map(convertFB).toList

    val connection = cnns.map(x => convertConnection(x, fbinsList)).toList

    (fbinsList, connection)
  }
}

