package gposition;

public class GCoups {

    public enum TYPE_DE_COUPS {

        Roque, EnPassant, Promotion, Deplacement, Prise, Attaque;
    }

    private static int ROI = 6;
    private static int e1 = 30, g1 = 32, c1 = 28, e8 = 114, g8 = 116, c8 = 112;
    private static int[] INDICECASES = {
            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2,
            3, 4, 5, 6, 7, -1, -1, -1, -1, 8, 9, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
            -1, -1, -1, 24, 25, 26, 27, 28, 29, 30, 31, -1, -1, -1, -1, 32, 33, 34, 35, 36, 37, 38, 39, -1, -1, -1, -1, 40,
            41, 42, 43, 44, 45, 46, 47, -1, -1, -1, -1, 48, 49, 50, 51, 52, 53, 54, 55, -1, -1, -1, -1, 56, 57, 58, 59, 60,
            61, 62, 63, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1
    };
    private static String[] STRING_CASES = {
            "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
            "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
            "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "a5", "b5", "c5", "d5",
            "e5", "f5", "g5", "h5", "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6", "a7", "b7", "c7", "d7", "e7", "f7",
            "g7", "h7", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"
    };
    private static String[] STRING_PIECE = {"", "N", "B", "R", "Q"};
    private final int PAS_DE_PIECE = -1;
    private final int piece;
    private final int caseO;
    private final int caseX;
    private final int pieceprise;
    private final TYPE_DE_COUPS type_de_coups;
    private final int piecePromotion;

    public GCoups(int piece, int caseO, int caseX, int pieceprise, TYPE_DE_COUPS type_de_coups) {
        this.piece = piece;
        this.caseO = caseO;
        this.caseX = caseX;
        this.pieceprise = pieceprise;
        this.type_de_coups = type_de_coups;
        this.piecePromotion = PAS_DE_PIECE;
    }

    public GCoups(int piece, int caseO, int caseX, int pieceprise, TYPE_DE_COUPS type_de_coups, int piecePromotion) {
        this.piece = piece;
        this.caseO = caseO;
        this.caseX = caseX;
        this.pieceprise = pieceprise;
        this.type_de_coups = type_de_coups;
        this.piecePromotion = piecePromotion;
    }

    public static String getString(GCoups coups) {
        if (((coups.piece == ROI) && (coups.caseO == e1) && (coups.caseX == g1))
                || ((coups.piece == ROI) && (coups.caseO == e8) && (coups.caseX == g8))) {
            return "O-O";
        } else if (((coups.piece == ROI) && (coups.caseO == e1) && (coups.caseX == c1))
                || ((coups.piece == ROI) && (coups.caseO == e8) && (coups.caseX == c8))) {
            return "O-O-O";
        } else if (coups.type_de_coups == TYPE_DE_COUPS.EnPassant) {
            return STRING_CASES[INDICECASES[coups.getCaseO()]] + "x" + STRING_CASES[INDICECASES[coups.getCaseX()]];
        } else if (coups.type_de_coups == TYPE_DE_COUPS.Promotion) {
            if (coups.pieceprise != 0) {
                return STRING_CASES[INDICECASES[coups.getCaseO()]] + "x" + STRING_CASES[INDICECASES[coups.getCaseX()]]
                        + STRING_PIECE[Math.abs(coups.piecePromotion)];
            } else {
                return STRING_CASES[INDICECASES[coups.getCaseO()]] + "-" + STRING_CASES[INDICECASES[coups.getCaseX()]]
                        + STRING_PIECE[Math.abs(coups.piecePromotion)];
            }
        } else {
            if (coups.type_de_coups == TYPE_DE_COUPS.Prise) {
                return STRING_CASES[INDICECASES[coups.getCaseO()]] + "x" + STRING_CASES[INDICECASES[coups.getCaseX()]];
            } else {
                return STRING_CASES[INDICECASES[coups.getCaseO()]] + "-" + STRING_CASES[INDICECASES[coups.getCaseX()]];
            }
        }
    }

    public int getCaseO() {
        return caseO;
    }

    public int getCaseX() {
        return caseX;
    }

    public int getPiece() {
        return piece;
    }

    public int getPiecePrise() {
        return pieceprise;
    }

    public TYPE_DE_COUPS getTypeDeCoups() {
        return type_de_coups;
    }

    public int getPiecePromotion() {
        return piecePromotion;
    }
}