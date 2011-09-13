package ideah.highlighter;

import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.ex.util.LayeredLexerEditorHighlighter;

public final class HaskellEditorHighlighter extends LayeredLexerEditorHighlighter {

    public HaskellEditorHighlighter(EditorColorsScheme scheme) {
        super(new HaskellSyntaxHighlighter(), scheme);
    }
}
