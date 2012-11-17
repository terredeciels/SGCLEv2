package gposition

import chesspresso.Chess
import org.apache.commons.collections.iterators.ArrayIterator
import java.util.ArrayList
import java.util.Collections
import scala.collection.JavaConversions._
import scalacode.TModele

class GPosition extends TModele{

  var fen: String = null
  var etats: Array[Int] = new Array[Int](NB_CELLULES)
  var traits: Int = 0
  var pseudocoups: ArrayList[GCoups] = null
  var cp_position: CPosition = null
  var droitPetitRoqueBlanc: Boolean = false
  var droitGrandRoqueNoir: Boolean = false
  var droitGrandRoqueBlanc: Boolean = false
  var droitPetitRoqueNoir: Boolean = false
  var caseEP: Int = 0

  final def init(fen: String) {
    this.fen = fen
    cp_position = new CPosition
    cp_position.init(fen)
    var caseO: Int = 0
    while (caseO < NB_CELLULES) {
      {
        etats(caseO) = OUT
      }
      ({
        caseO += 1;
        caseO - 1
      })
    }

    val itetats: ArrayIterator = new ArrayIterator(cp_position.getEtats)
    var indice: Int = 0
    while (itetats.hasNext) {
      val e: Integer = itetats.next.asInstanceOf[Integer]
      etats(CASES(indice)) = e
      indice += 1
    }
    if (cp_position.getTrait == Chess.WHITE) {
      traits = BLANC
    }
    else {
      traits = NOIR
    }
    droitPetitRoqueBlanc = cp_position.getDroitPetitRoqueBlanc
    droitPetitRoqueNoir = cp_position.getDroitPetitRoqueNoir
    droitGrandRoqueBlanc = cp_position.getDroitGrandRoqueBlanc
    droitGrandRoqueNoir = cp_position.getDroitGrandRoqueNoir
    if (cp_position.getCaseEP == PAS_DE_CASE) {
      caseEP = -1
    }
    else {
      caseEP = CASES(cp_position.getCaseEP)
    }
  }
  def setPseudocoups(pseudocoups: ArrayList[GCoups]) {
    this.pseudocoups = pseudocoups
  }
  def getEtats: Array[Int] = {
    return etats
  }
  def getTrait: Int = {
    return traits
  }
  def getCaseEP: Int = {
    return caseEP
  }
  def getDroitPetitRoqueBlanc: Boolean = {
    return droitPetitRoqueBlanc
  }
  def getDroitPetitRoqueNoir: Boolean = {
    return droitPetitRoqueNoir
  }
  def getDroitGrandRoqueNoir: Boolean = {
    return droitGrandRoqueNoir
  }
  def getDroitGrandRoqueBlanc: Boolean = {
    return droitGrandRoqueBlanc
  }
  private def toStringListGCoups: ArrayList[String] = {
    val result: ArrayList[String] = new ArrayList[String]

    for (c <- pseudocoups) {
      result.add(c.getString(c))
    }
    Collections.sort(result)
    return result
  }
  def copie: GPosition = {
    val position: GPosition = new GPosition
    System.arraycopy(etats, 0, position.etats, 0, NB_CELLULES)
    return position
  }
  override def toString: String = {
    val diff: ArrayList[String] = getTest.getDiffStringList
    if (!diff.isEmpty) {
      return fen + '\n' + "Coups ChessPresso:" + "\n" + cp_position.toStringListCPCoups + '\n' + "Coups GCLE:" + "\n" + toStringListGCoups + "\n" + "Diff:" + "\n" + diff + "\n"
    }
    else {
      return ""
    }
  }
  private def getTest: GPositionTest = {
    val valid: GPositionTest = new GPositionTest
    val lg_coups: ArrayList[String] = toStringListGCoups
    val lcp_coups: ArrayList[String] = cp_position.toStringListCPCoups
    if (lg_coups.size <= lcp_coups.size) {
      valid.setDiffStringList(getDiff(lg_coups, lcp_coups))
    }
    else {
      valid.setDiffStringList(getDiff(lcp_coups, lg_coups))
    }
    return valid
  }
  private def getDiff(L1: ArrayList[String], L2: ArrayList[String]): ArrayList[String] = {
    L2.removeAll(L1)
    return L2
  }

}