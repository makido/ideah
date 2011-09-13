package haskell.parser;

import com.intellij.psi.PsiFile;

public interface HaskellFile extends PsiFile {

    boolean isMainModule();
}
