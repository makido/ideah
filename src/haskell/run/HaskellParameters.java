package haskell.run;

import com.intellij.execution.configurations.SimpleProgramParameters;
import com.intellij.openapi.projectRoots.Sdk;
import haskell.parser.HaskellFile;

final class HaskellParameters extends SimpleProgramParameters {

    private Sdk ghc;
    private HaskellFile mainFile;

    public Sdk getGhc() {
        return ghc;
    }

    public void setGhc(Sdk ghc) {
        this.ghc = ghc;
    }

    public HaskellFile getMainFile() {
        return mainFile;
    }

    public void setMainFile(HaskellFile mainFile) {
        this.mainFile = mainFile;
    }
}
