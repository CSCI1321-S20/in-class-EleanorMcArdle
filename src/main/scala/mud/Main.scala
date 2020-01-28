package mud

import java.awt.event.ItemEvent

object Main {
    def main(args: Array[String]):Unit = {
      var input = readLine()
      val player = new Player("dude", List[Item]())
      while (input != "exit"){
        player.processCommand(input)
        input = readLine()
      }
    }
}