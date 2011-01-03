package haskell;

import com.intellij.lang.Language;

public final class HaskellLanguage extends Language {

    public HaskellLanguage() {
        super("Haskell");
    }

    @Override
    public boolean isCaseSensitive() {
        return true;
    }
}
