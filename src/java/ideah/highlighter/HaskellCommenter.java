package ideah.highlighter;

import com.intellij.lang.CodeDocumentationAwareCommenter;
import com.intellij.psi.PsiComment;
import com.intellij.psi.tree.IElementType;
import ideah.lexer.HaskellTokenTypes;

public final class HaskellCommenter implements CodeDocumentationAwareCommenter, HaskellTokenTypes {

    public String getLineCommentPrefix() {
        return "--";
    }

    public String getBlockCommentPrefix() {
        return "{-";
    }

    public String getBlockCommentSuffix() {
        return "-}";
    }

    public String getCommentedBlockCommentPrefix() {
        return null;
    }

    public String getCommentedBlockCommentSuffix() {
        return null;
    }

    public IElementType getLineCommentTokenType() {
        return COMMENT;
    }

    public IElementType getBlockCommentTokenType() {
        return ML_COMMENT;
    }

    public IElementType getDocumentationCommentTokenType() {
        return null;
    }

    public String getDocumentationCommentPrefix() {
        return null;
    }

    public String getDocumentationCommentLinePrefix() {
        return null;
    }

    public String getDocumentationCommentSuffix() {
        return null;
    }

    public boolean isDocumentationComment(PsiComment element) {
        return false;
    }
}
