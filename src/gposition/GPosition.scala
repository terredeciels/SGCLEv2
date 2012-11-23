package gposition

import chesspresso._
import scalacode.TModele
import collection.mutable.ArrayBuffer

class GPosition extends TModele {

  var fen: String = _
  var pseudocoups: ArrayBuffer[GCoups] = _
  var cp_position: CPosition = _
  var droitPetitRoqueBlanc = false
  var droitGrandRoqueNoir = false
  var droitGrandRoqueBlanc = false
  var droitPetitRoqueNoir = false
  var caseEP = 0
  var traits = 0
  var etats = new Array[Int](NB_CELLULES)

  final def init(fen: String) {
    cp_position = new CPosition
    cp_position.init(fen)
    for (caseO <- 0 to NB_CELLULES - 1) etats(caseO) = OUT
    val itetats = cp_position.etats.iterator
    var indice = 0
    while (itetats.hasNext) {
      etats(CASES(indice)) = itetats.next.asInstanceOf[java.lang.Integer]
      indice += 1
    }
    if (cp_position.traits == Chess.WHITE) traits = BLANC
    else traits = NOIR

    droitPetitRoqueBlanc = cp_position.droitPetitRoqueBlanc
    droitPetitRoqueNoir = cp_position.droitPetitRoqueNoir
    droitGrandRoqueBlanc = cp_position.droitGrandRoqueBlanc
    droitGrandRoqueNoir = cp_position.droitGrandRoqueNoir

    if (cp_position.caseEP == PAS_DE_CASE) caseEP = -1
    else caseEP = CASES(cp_position.caseEP)
  }

  def toStringListGCoups = {
    val result = new ArrayBuffer[String]
    for (c <- pseudocoups) result += c.getString
    result.sorted
  }

  def copie = {
    val position = new GPosition
    System.arraycopy(etats, 0, position.etats, 0, NB_CELLULES)
    position
  }
  override def toString = {
    if (! diffStringList.isEmpty) {
      fen + '\n' + "Coups ChessPresso:" + "\n" + cp_position.toStringListCPCoups
      +'\n' + "Coups GCLE:" + "\n" + toStringListGCoups + "\n" + "Diff:" + "\n" + diffStringList + "\n"
    }
    else {
      ""
      // + cp_position.toStringListCPCoups + "\n"
      // //+ toStringListGCoups + "\n"
    }
  }
  def diffStringList = {
    val lg_coups = toStringListGCoups
    val lcp_coups = cp_position.toStringListCPCoups
    var diff = new ArrayBuffer[String]
   diff = lg_coups --= lcp_coups
    diff
  }


}