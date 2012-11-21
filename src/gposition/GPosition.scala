package gposition

import chesspresso._
import java.util._
import org.apache.commons.collections.iterators._
import scalacode.TModele
import scala.collection.JavaConversions._

class GPosition extends TModele {

  var fen: String = _
  var etats: Array[Int] = _
  var traits = 0
  var pseudocoups: ArrayList[GCoups] = _
  var cp_position: CPosition = _
  var droitPetitRoqueBlanc = false
  var droitGrandRoqueNoir = false
  var droitGrandRoqueBlanc = false
  var droitPetitRoqueNoir = false
  var caseEP = 0
  etats = new Array[Int](NB_CELLULES)

  final def init(fen: String) {
    this.fen = fen
    cp_position = new CPosition
    cp_position.init(fen)
    var caseO = 0
    while (caseO < NB_CELLULES) {
      {
        etats(caseO) = OUT
      }
      ({
        caseO += 1;
        caseO - 1
      })
    }

    val itetats = new ArrayIterator(cp_position.etats)
    var indice = 0
    while (itetats.hasNext) {
      val e: Integer = itetats.next.asInstanceOf[Integer]
      etats(CASES(indice)) = e
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
    val result = new ArrayList[String]
    for (c <- pseudocoups) result.add(c.getString)
    Collections.sort(result)
    result
  }

  def copie = {
    val position = new GPosition
    System.arraycopy(etats, 0, position.etats, 0, NB_CELLULES)
    position
  }
  override def toString = {
    if (!diffStringList.isEmpty) {
      fen + '\n' + "Coups ChessPresso:" + "\n" + cp_position.toStringListCPCoups
    +'\n' + "Coups GCLE:" + "\n" + toStringListGCoups + "\n" + "Diff:" + "\n" + diffStringList + "\n"   }
    else  {
     ""
     // + cp_position.toStringListCPCoups + "\n"
    }
    //""
    //+ toStringListGCoups + "\n"

  }
  def diffStringList = {
    val lg_coups = toStringListGCoups
    val lcp_coups = cp_position.toStringListCPCoups
    var diff = new ArrayList[String]
    if (lg_coups.size <= lcp_coups.size) {
      diff = getDiff(lg_coups, lcp_coups)
    }
    else {
      diff = getDiff(lcp_coups, lg_coups)
    }
    diff
  }
  def getDiff(L1: ArrayList[String], L2: ArrayList[String]) = {
    L2.removeAll(L1)
    L2
  }

}