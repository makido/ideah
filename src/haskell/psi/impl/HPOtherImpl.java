package haskell.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.StubElement;
import haskell.psi.api.HPOther;
import org.jetbrains.annotations.NotNull;

public final class HPOtherImpl extends HaskellBaseElementImpl<HPOther, StubElement<HPOther>> {

    public HPOtherImpl(@NotNull ASTNode node) {
        super(node);
    }
}
