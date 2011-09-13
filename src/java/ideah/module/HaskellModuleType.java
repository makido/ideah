package ideah.module;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.IconLoader;
import ideah.HaskellFileType;

import javax.swing.*;

public final class HaskellModuleType extends ModuleType<HaskellModuleBuilder> {

    public static final HaskellModuleType INSTANCE = new HaskellModuleType();

    public HaskellModuleType() {
        super("HASKELL_MODULE");
    }

    public HaskellModuleBuilder createModuleBuilder() {
        return new HaskellModuleBuilder();
    }

    public String getName() {
        return "Haskell Project";
    }

    public String getDescription() {
        return "<b>Haskell</b> project"; // todo: more wordy!
    }

    public Icon getBigIcon() {
        return IconLoader.getIcon("/ideah/haskell_24x24.png");
    }

    public Icon getNodeIcon(boolean isOpened) {
        return HaskellFileType.HASKELL_ICON; // todo: another icon?
    }

    @Override
    public boolean isValidSdk(Module module, Sdk projectSdk) {
        return true; // todo
    }
}
