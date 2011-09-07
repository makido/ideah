package haskell.util;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;

public class Paths {

        public static VirtualFile getLibPath(Module module) {
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null)
            return null;
        VirtualFile sdkHome = sdk.getHomeDirectory();
        if (sdkHome == null)
            return null;
        return sdkHome.findChild("lib");
    }
}
