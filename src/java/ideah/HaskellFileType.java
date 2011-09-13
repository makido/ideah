package ideah;

import com.intellij.lang.Language;
import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.vfs.VirtualFile;
import ideah.highlighter.HaskellEditorHighlighter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.nio.charset.Charset;

public final class HaskellFileType extends LanguageFileType {

    public static final HaskellFileType INSTANCE = new HaskellFileType();
    public static final Language HASKELL_LANGUAGE = INSTANCE.getLanguage();
    public static final Icon HASKELL_ICON = IconLoader.getIcon("/ideah/haskell_16x16.png");

    public HaskellFileType() {
        super(new HaskellLanguage());
    }

    @NotNull
    public String getName() {
        return "Haskell";
    }

    @NotNull
    public String getDescription() {
        return "Haskell files";
    }

    @NotNull
    public String getDefaultExtension() {
        return "hs";
    }

    public Icon getIcon() {
        return HASKELL_ICON;
    }

    public boolean isJVMDebuggingSupported() {
        return false;
    }

    @Override
    public EditorHighlighter getEditorHighlighter(@Nullable Project project,
                                                  @Nullable VirtualFile virtualFile,
                                                  @NotNull EditorColorsScheme colors) {
        return new HaskellEditorHighlighter(colors);
    }

    @Override
    public String getCharset(@NotNull VirtualFile file, byte[] content) {
        return "UTF8";
    }

    @Override
    public Charset extractCharsetFromFileContent(@Nullable Project project, @Nullable VirtualFile file, @NotNull String content) {
        return Charset.forName("UTF8");
    }
}
