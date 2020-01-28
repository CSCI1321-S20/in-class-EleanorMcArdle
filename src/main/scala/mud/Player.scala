package mud
// "college is a large problem" - M. Lewis 1/28/20

class Player(val name:String, private var items: List[Item]) {
  private var location = Room.rooms(0)

def processCommand(input: String): Unit = {
    val command = input.trim.toLowerCase()
    command match {
        case "north" | "south" | "east" | "west" | "up" | "down" | "n" | "s" | "e" | "w" | "u" | "d" => move(command)
        case "look" => println(location.description())
        case "inv" => println(inventoryListing())
        case g if g.contains("get") => {
            location.getItem(g.trim.split(" ")(1)) match {
                case Some(item) => addToInventory(item)
                case None => println("item does not exist")
                }       
        }
        case d if d.contains("drop") => {
            getFromInventory(d.trim.split(" ")(1)) match {
                 case Some(item) => location.dropItem(item)
                case None => println("item does not exist")
                }  
        }
        case "help" => helpMe()
        case _ => println("ya done goofed up")
    }
}


def getFromInventory(itemName: String): Option[Item] = {
    items.find(_.name.toLowerCase == itemName.toLowerCase) match {
      case Some(item) =>
        items = items.filter(_ != item)
        Some(item)
      case None => None
    }
}

def helpMe(): Unit= {
  println(""" north, south, east, west, up, down - move in the indicated direction
look - reprints the description of the current room
inv - list the contents of your inventory
get item - to get an item from the room and add it to your inventory
drop item - to drop an item from your inventory into the room.
exit - leave the game
help - print the available commands and what they do""" )
}

def addToInventory(item: Item): Unit = items ::= item

def inventoryListing(): String = {
  var stuff = List[String]()
  if (items.length > 0 ) {
  for(i <- 0 until items.length){
  stuff = items(i).name + " - " + items(i).desc :: stuff}
  "Inventory: \n" + stuff.mkString("\n")
  }
  else "you dont have anything in your inventory"
}

def move(dir: String): Unit = {
  {dir match {
    case "north" | "n" => location.getExit(0)
    case "south" | "s" => location.getExit(1)
    case "east" | "e" => location.getExit(2)
    case "west" | "w" => location.getExit(3)
    case "up" | "u" => location.getExit(4)
    case "down" | "d" => location.getExit(5)
  }} match {
    case Some(room) => {location = room
      println(location.description())
    }
    case None => println("cannot go that way")
  }
}

}