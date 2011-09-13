package ideah.highlighter;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import ideah.lexer.HaskellTokenTypes;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public final class HaskellBraceMatcher implements PairedBraceMatcher, HaskellTokenTypes {

    private static final BracePair[] PAIRS = new BracePair[] {
        new BracePair(L_PAREN, R_PAREN, true),
        new BracePair(L_SQUARE, R_SQUARE, true),
        new BracePair(L_CURLY, R_CURLY, true)
    };

    public BracePair[] getPairs() {
        return PAIRS;
    }

    public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
        return contextType == null
            || WHITESPACES.contains(contextType)
            || COMMENTS.contains(contextType)
            || contextType == COMMA
            || contextType == R_SQUARE
            || contextType == R_PAREN
            || contextType == R_CURLY;
    }

    public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
        return openingBraceOffset;
    }
}
