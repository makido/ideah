package haskell.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.StubElement;
import haskell.psi.api.HPModule;
import org.jetbrains.annotations.NotNull;

public final class HPModuleImpl extends HaskellBaseElementImpl<HPModule, StubElement<HPModule>> implements HPModule {

    public HPModuleImpl(@NotNull ASTNode node) {
        super(node);
    }
}
