package gposition

import scalacode.TModele
import gposition.TypeDeCoups._

class GCoups(var piece: Int,var caseO: Int,var caseX: Int,var pieceprise: Int,
             var typedecoups: TypeDeCoups, var piecePromotion: Int) extends TModele {


  def this(piece: Int, caseO: Int, caseX: Int, pieceprise: Int, typedecoups: TypeDeCoups) =
    this(piece, caseO, caseX, pieceprise, typedecoups, 0)

  def getString(coups: GCoups): String = {
    if ((coups.piece == ROI && coups.caseO == e1 && coups.caseX == g1)
      || (coups.piece == ROI && coups.caseO == e8 && coups.caseX == g8)) {
      return "O-O"
    }
    else if ((coups.piece == ROI && coups.caseO == e1 && coups.caseX == c1)
      || (coups.piece == ROI && coups.caseO == e8 && coups.caseX == c8)) {
      return "O-O-O"
    }
    else if (coups.typedecoups == TypeDeCoups.EnPassant) {
      return STRING_CASES(INDICECASES(coups.caseO)) + "x" + STRING_CASES(INDICECASES(coups.caseX))
    }
    else if (coups.typedecoups == TypeDeCoups.Promotion) {
      if (coups.pieceprise != 0) {
        return STRING_CASES(INDICECASES(coups.caseO)) + "x" + STRING_CASES(INDICECASES(coups.caseX)) + STRING_PIECE(Math.abs(coups.piecePromotion))
      }
      else {
        return STRING_CASES(INDICECASES(coups.caseO)) + "-" + STRING_CASES(INDICECASES(coups.caseX)) + STRING_PIECE(Math.abs(coups.piecePromotion))
      }
    }
    else {
      if (coups.typedecoups == TypeDeCoups.Prise) {
        return STRING_CASES(INDICECASES(coups.caseO)) + "x" + STRING_CASES(INDICECASES(coups.caseX))
      }
      else {
        return STRING_CASES(INDICECASES(coups.caseO)) + "-" + STRING_CASES(INDICECASES(coups.caseX))
      }
    }
  }

}