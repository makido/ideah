package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import ideah.psi.api.HPModule;
import org.jetbrains.annotations.NotNull;

public final class HPModuleImpl extends HaskellBaseElementImpl implements HPModule {

    public HPModuleImpl(@NotNull ASTNode node) {
        super(node);
    }
}
