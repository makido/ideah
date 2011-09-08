package haskell.util;

import com.intellij.openapi.module.Module;

import java.io.File;

public class FileNames {

    public static final String ERR_TEST = "err_test";

    public static String getExeName(String file) {
        String os = System.getProperty("os.name");
        return os.toLowerCase().contains("windows")
                ? file + ".exe"
                : file;
    }

    public static String getHsName(String file) {
        return file + ".hs";
    }

    public static String getFullErrTestHsName() {
        return new File(Paths.getPluginPath(), getHsName(ERR_TEST)).getAbsolutePath();
    }

    public static String getFullErrTestExeName() {
        return new File(Paths.getPluginPath(), getExeName(ERR_TEST)).getAbsolutePath();
    }

    public static String getCompilerPath(Module module) {
        return new File(Paths.getBinVFile(module).getPath(), "ghc").getAbsolutePath();
    }
}
