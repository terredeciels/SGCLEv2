package gposition

import chesspresso.move.Move
import chesspresso.position.Position
import java.util.ArrayList
import java.util.Collections

final class CPosition {

  private final val NB_CASES: Int = 64
  private final val CASES: Array[Int] = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63)
  private var etats: Array[Int] = null
  private var coups: Array[Short] = null
  private var traits: Int = 0
  private var droitPetitRoqueBlanc: Boolean = false
  private var droitGrandRoqueNoir: Boolean = false
  private var droitGrandRoqueBlanc: Boolean = false
  private var droitPetitRoqueNoir: Boolean = false
  private var caseEP: Int = 0

  final def init(fen: String) {
    etats = new Array[Int](NB_CASES)
    val position: Position = new Position(fen)
    for (caseO <- CASES) {
      etats(caseO) = position.getStone(caseO)
    }
    coups = position.getAllMoves
    traits = position.getToPlay
    val roques: Int = position.getCastles
    setPetitRoqueNoir((8 & roques) == 8)
    setGrandRoqueNoir((4 & roques) == 4)
    setPetitRoqueBlanc((2 & roques) == 2)
    setGrandRoqueBlanc((1 & roques) == 1)
    caseEP = position.getSqiEP
  }
  def getEtats: Array[Int] = {
    return etats
  }
  def getTrait: Int = {
    return traits
  }
  protected def getCoups: Array[Short] = {
    return coups
  }
  def getCaseEP: Int = {
    return caseEP
  }
  def getDroitPetitRoqueBlanc: Boolean = {
    return droitPetitRoqueBlanc
  }
  def getDroitGrandRoqueNoir: Boolean = {
    return droitGrandRoqueNoir
  }
  def getDroitGrandRoqueBlanc: Boolean = {
    return droitGrandRoqueBlanc
  }
  def getDroitPetitRoqueNoir: Boolean = {
    return droitPetitRoqueNoir
  }
  private def setPetitRoqueBlanc(b: Boolean) {
    this.droitPetitRoqueBlanc = b
  }
  private def setGrandRoqueNoir(b: Boolean) {
    this.droitGrandRoqueNoir = b
  }
  private def setPetitRoqueNoir(b: Boolean) {
    this.droitPetitRoqueNoir = b
  }
  private def setGrandRoqueBlanc(b: Boolean) {
    this.droitGrandRoqueBlanc = b
  }
  def toStringListCPCoups: ArrayList[String] = {
    val result: ArrayList[String] = new ArrayList[String]
    for (c <- coups) {
      result.add(Move.getString(c))
    }
    Collections.sort(result)
    return result
  }

}