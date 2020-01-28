 
package mud

import scala.languageFeature.existentials

class Room(val name: String, val desc: String, private var items: List[Item], private val exits: Array[Int]) {
//fix exits thats dumb 

  def description(): String = name + "\n" + desc + "\nexits: " + exitsToWords + "\nitems: " + itemsToWords()
  

  def exitsToWords(): String = {
       var locs = List[String]()
    for (i <- 0 until exits.length){
      if (exits(i) >= 0){
        i match{
          case 0 => locs = "north" :: locs
          case 1 => locs = "south" :: locs
          case 2 => locs = "east" :: locs
          case 3 => locs = "west" :: locs
          case 4 => locs = "up" :: locs
          case 5 => locs = "down" :: locs
        }
      }
    }
    locs.mkString(", ")
  }

  def itemsToWords():String = {
    var names = List[String]()
    for (i <- 0 until items.length){
      names = items(i).name :: names
    }
    names.mkString(", ")
  }

  def getExit(dir: Int): Option[Room] ={
    if (exits(dir) >= 0 ){
      Some(Room.rooms(exits(dir)))
    }
    else None
  }

  def getItem(itemName: String): Option[Item] = {
    items.find(_.name.toLowerCase == itemName.toLowerCase) match {
      case Some(item) =>
        items = items.filter(_ != item)
        Some(item)
      case None => None
    }
  }

  def dropItem(item: Item): Unit = items ::= item
}

object Room {
  val rooms = readRooms()

  def readRooms(): Array[Room] = {
    val source = scala.io.Source.fromFile("world.txt")
    val lines = source.getLines()
    val r = Array.fill(lines.next.toInt)(readRoom(lines))
    source.close()
    r
  }

  def readRoom(lines: Iterator[String]): Room = {
    val name = lines.next()
    val desc = lines.next()
    val items = List.fill(lines.next.toInt)(Item(lines.next(), lines.next()))
    val exits = lines.next().split(",").map(_.toInt)
    new Room(name, desc, items, exits)
  }
  
}