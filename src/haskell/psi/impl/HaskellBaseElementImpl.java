package haskell.psi.impl;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import haskell.psi.HaskellPsiElement;
import org.jetbrains.annotations.NotNull;

public abstract class HaskellBaseElementImpl<E extends HaskellPsiElement, T extends StubElement<E>> extends StubBasedPsiElementBase<T> implements HaskellPsiElement {

    protected HaskellBaseElementImpl(@NotNull ASTNode node) {
        super(node);
    }

    protected HaskellBaseElementImpl(@NotNull T stub, @NotNull IStubElementType<T, E> nodeType) {
        super(stub, nodeType);
    }
}
