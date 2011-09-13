package haskell.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import haskell.lexer.HaskellTokenTypes;
import org.jetbrains.annotations.NotNull;

public final class HaskellParser implements PsiParser, HaskellElementTypes {

    @NotNull
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        builder.setDebugMode(true);
        PsiBuilder.Marker start = builder.mark();
        while (true) {
            IElementType type = builder.getTokenType();
            if (type == null)
                break;
            if (type == HaskellTokenTypes.VAR_ID) {
                PsiBuilder.Marker idMark = builder.mark();
                builder.advanceLexer();
                idMark.done(type);
            } else {
                builder.advanceLexer();
            }
        }
        start.done(root);
        return builder.getTreeBuilt();
    }
}
