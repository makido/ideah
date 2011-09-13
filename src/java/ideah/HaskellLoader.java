package ideah;

import com.intellij.openapi.components.ApplicationComponent;
import org.jetbrains.annotations.NotNull;

public final class HaskellLoader implements ApplicationComponent {

    @NotNull
    public String getComponentName() {
        return "haskell.support.loader";
    }

    public void initComponent() {
        // todo
    }

    public void disposeComponent() {
    }
}
