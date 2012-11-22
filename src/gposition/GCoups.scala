package gposition

import scalacode.TModele
import gposition.TypeCoups._


class GCoups(var piece: Int, var caseO: Int, var caseX: Int, var pieceprise: Int,
             var typedecoups: TypeCoups, var piecePromotion: Int) extends TModele {

  def this(piece: Int, caseO: Int, caseX: Int, pieceprise: Int, typedecoups: TypeCoups) =
    this(piece, caseO, caseX, pieceprise, typedecoups, 0)

  def getString: String = {
    if (((piece == ROI) && (caseO == e1) && (caseX == g1)) || ((piece == ROI) && (caseO == e8) && (caseX == g8))) {
      return "O-O"
    }
    else if (((piece == ROI) && (caseO == e1) && (caseX == c1)) || ((piece == ROI) && (caseO == e8) && (caseX == c8))) {
      return "O-O-O"
    }
    else if (typedecoups == TypeCoups.EnPassant) {
      return STRING_CASES(INDICECASES(caseO)) + "x" + STRING_CASES(INDICECASES(caseX))
    }
    else if (typedecoups == TypeCoups.Promotion) {
      if (pieceprise != 0) {
        return STRING_CASES(INDICECASES(caseO)) + "x" + STRING_CASES(INDICECASES(caseX)) + STRING_PIECE(Math.abs(piecePromotion))
      }
      else {
        return STRING_CASES(INDICECASES(caseO)) + "-" + STRING_CASES(INDICECASES(caseX)) + STRING_PIECE(Math.abs(piecePromotion))
      }
    }
    else {
      if (typedecoups == TypeCoups.Prise) {
        return STRING_CASES(INDICECASES(caseO)) + "x" + STRING_CASES(INDICECASES(caseX))
      }
      else {
        return STRING_CASES(INDICECASES(caseO)) + "-" + STRING_CASES(INDICECASES(caseX))
      }
    }
  }

}