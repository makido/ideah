package haskell.parser;

import com.intellij.lang.ASTFactory;
import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PlainTextTokenTypes;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiUtilBase;
import haskell.HaskellFileType;
import haskell.lexer.HaskellLexer;
import haskell.lexer.HaskellTokenTypes;
import org.jetbrains.annotations.NotNull;

public final class HaskellParserDefinition implements ParserDefinition, HaskellTokenTypes {

    public static final IFileElementType HASKELL_FILE = new IFileElementType(HaskellFileType.HASKELL_LANGUAGE) {
        public ASTNode parseContents(ASTNode chameleon) {
            CharSequence chars = chameleon.getChars();
            return ASTFactory.leaf(PlainTextTokenTypes.PLAIN_TEXT, chars); // todo
        }
    };

    @NotNull
    public Lexer createLexer(Project project) {
        return new HaskellLexer();
    }

    public PsiParser createParser(Project project) {
        throw new UnsupportedOperationException("Not supported");
    }

    public IFileElementType getFileNodeType() {
        return HASKELL_FILE;
    }

    @NotNull
    public TokenSet getWhitespaceTokens() {
        return WHITESPACES;
    }

    @NotNull
    public TokenSet getCommentTokens() {
        return COMMENTS;
    }

    @NotNull
    public TokenSet getStringLiteralElements() {
        return STRINGS;
    }

    @NotNull
    public PsiElement createElement(ASTNode node) {
        return PsiUtilBase.NULL_PSI_ELEMENT;
    }

    public PsiFile createFile(FileViewProvider viewProvider) {
        return new HaskellFileImpl(viewProvider);
    }

    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
        return SpaceRequirements.MAY;
    }
}
