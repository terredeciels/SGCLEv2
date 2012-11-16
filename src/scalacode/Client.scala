package scalacode

import main.Fen
import main.Ui
import gposition.GPosition
import scala.collection.JavaConversions._

object Client {

  def main(args: Array[String]) {
    val position: GPosition = GPosition.getInstance
    val command: Array[String] = new Array[String](3)
    command(0) = "-cli"
    // command(1) = "F:/ProgmEchecsNotes/shirov.pgn" //814
    // command(1) = "F:/ProgmEchecsNotes/paulsen.pgn"; //322
    command(1) = "F:/ProgmEchecsNotes/ashley.pgn"; //414
    //    command[1] = "F:/ProgmEchecsNotes/bird.pgn";//353
    //        command[1] = "F:/ProgmEchecsNotes/Tartakower.pgn";//1290
    // command(1) = "F:/ProgmEchecsNotes/Capablanca.pgn"; //597
    //  command(1) = "F:/ProgmEchecsNotes/Boleslavsky.pgn"; //651
    //       command(1) = "F:/ProgmEchecsNotes/Soltis.pgn";//370
    // command(1) = "F:/ProgmEchecsNotes/Motylev.pgn";//1169

    Ui.main(command)
    for (f <- Fen.getFenList) {
      position.init(f)
      position.setPseudocoups(new Generateur(position).fCoupsLegaux)
      //position.setPseudocoups(fCoupsLegaux(position))
      System.out.print(position)
    }
  }
}