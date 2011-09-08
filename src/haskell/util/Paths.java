package haskell.util;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;

import java.io.File;

public class Paths {

    public static final String getPluginPath() {
        return new File(System.getProperty("user.home"), ".ideah").getAbsolutePath();
    }

    public static VirtualFile getLibVFile(Module module) {
        return getSomeVFile(module, "lib");
    }

    public static VirtualFile getBinVFile(Module module) {
        return getSomeVFile(module, "bin");
    }

    private static VirtualFile getSomeVFile(Module module, String dirName) {
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null)
            return null;
        VirtualFile sdkHome = sdk.getHomeDirectory();
        if (sdkHome == null)
            return null;
        return sdkHome.findChild(dirName);
    }
}
