package gposition

import chesspresso.move.Move
import chesspresso.position.Position
import java.util.{Collections, ArrayList}
import scalacode.TModele

class CPosition extends TModele {

  var etats: Array[Int] = _
  var coups: Array[Short] = _
  var traits = 0
  var droitPetitRoqueBlanc = false
  var droitGrandRoqueNoir = false
  var droitGrandRoqueBlanc = false
  var droitPetitRoqueNoir = false
  var caseEP = 0

  final def init(fen: String) {
    etats = new Array[Int](NB_CASES)
    val position = new Position(fen)
    for (caseO <- CP_CASES) etats(caseO) = position.getStone(caseO)
    coups = position.getAllMoves
    traits = position.getToPlay
    val roques = position.getCastles
    droitPetitRoqueNoir = (8 & roques) == 8
    droitGrandRoqueNoir = (4 & roques) == 4
    droitPetitRoqueBlanc = (2 & roques) == 2
    droitGrandRoqueBlanc = (1 & roques) == 1
    caseEP = position.getSqiEP
  }

  def toStringListCPCoups = {
    val result = new ArrayList[String]
    for (c <- coups) result.add(Move.getString(c))
    Collections.sort(result)
    result
  }
//  def toIntListCPCoups = {
//    val result = new ArrayList[String]
//    for (c <- coups) result.add( Move.getBinaryString(c))
//    Collections.sort(result)
//    result
//  }
}