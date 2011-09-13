package ideah.lexer;

import com.intellij.lexer.LexerBase;
import com.intellij.psi.tree.IElementType;

public final class HaskellLexer extends LexerBase {

    private final HaskellLexerImpl lexer;

    private CharSequence text = null;
    private int end;
    private HaskellToken token = null;

    public HaskellLexer() {
        this.lexer = new HaskellLexerImpl();
    }

    private void nextToken() {
        token = lexer.nextToken();
    }

    public void start(CharSequence buffer, int startOffset, int endOffset, int initialState) {
        text = buffer;
        end = endOffset;
        lexer.init(buffer, startOffset, endOffset);
        token = null;
        nextToken();
    }

    public int getState() {
        return 0;
    }

    public IElementType getTokenType() {
        return token == null ? null : token.type;
    }

    public int getTokenStart() {
        return token == null ? end : token.coords;
    }

    public int getTokenEnd() {
        return token == null ? end : token.coords + token.text.length();
    }

    public void advance() {
        nextToken();
    }

    public CharSequence getBufferSequence() {
        return text;
    }

    public int getBufferEnd() {
        return end;
    }
}
