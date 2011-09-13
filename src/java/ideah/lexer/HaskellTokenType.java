package ideah.lexer;

import com.intellij.psi.tree.IElementType;
import ideah.HaskellFileType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public final class HaskellTokenType extends IElementType {

    private final String debugName;

    public HaskellTokenType(@NotNull @NonNls String debugName) {
        super(debugName, HaskellFileType.HASKELL_LANGUAGE);
        this.debugName = debugName;
    }

    @Override
    public String toString() {
        return debugName;
    }
}
