package haskell.util;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URISyntaxException;

public final class CompilerLocation {

    private static final Logger LOG = Logger.getInstance("haskell.util.Paths");
    private static final String ERR_TEST = "err_test";

    private static boolean freshCheckDone = false;

    public final String exe;
    public final String libPath;

    private CompilerLocation(String exe, String libPath) {
        this.exe = exe;
        this.libPath = libPath;
    }

    private static String getExeName(String file) {
        return SystemInfo.isWindows
                ? file + ".exe"
                : file;
    }

    private static VirtualFile getSdkHome(Module module) {
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null)
            return null;
        return sdk.getHomeDirectory();
    }

    private static File[] listHaskellSources() throws URISyntaxException {
        File pluginHaskellDir = new File(CompilerLocation.class.getResource("/haskell/").toURI());
        return pluginHaskellDir.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.endsWith(".hs");
            }
        });
    }

    private static boolean needRecompile(File compilerExe) throws URISyntaxException {
        if (freshCheckDone)
            return false;
        if (compilerExe.exists()) {
            long exeLastModified = compilerExe.lastModified();
            File[] haskellSources = listHaskellSources();
            for (File file : haskellSources) {
                if (file.lastModified() > exeLastModified)
                    return true;
            }
            freshCheckDone = true;
            return false;
        } else {
            return true;
        }
    }

    public static synchronized CompilerLocation get(Module module) {
        VirtualFile ghcHome = getSdkHome(module);
        if (ghcHome == null)
            return null;
        VirtualFile ghcLib = ghcHome.findChild("lib");
        if (ghcLib == null)
            return null;
        try {
            File pluginPath = new File(System.getProperty("user.home"), ".ideah");
            pluginPath.mkdirs();
            File compilerExe = new File(pluginPath, getExeName(ERR_TEST));
            if (needRecompile(compilerExe)) {
                if (!compileHs(pluginPath, ghcHome, compilerExe))
                    return null;
            }
            if (compilerExe.exists()) {
                return new CompilerLocation(compilerExe.getAbsolutePath(), ghcLib.getPath());
            } else {
                return null;
            }
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }

    public static String rootsToString(VirtualFile[] roots) {
        StringBuilder sourceRoots = new StringBuilder();
        for (VirtualFile root : roots) {
            sourceRoots.append(":").append(root.getPath());
        }
        return sourceRoots.substring(1);
    }

    private static boolean compileHs(File pluginPath, VirtualFile ghcHome, File exe) throws IOException, InterruptedException, URISyntaxException {
        VirtualFile ghcBin = ghcHome.findChild("bin");
        if (ghcBin == null)
            return false;
        File[] haskellSources = listHaskellSources();
        for (File file : haskellSources) {
            File outFile = new File(pluginPath, file.getName());
            FileUtil.copy(file, outFile);
        }
        String mainHs = ERR_TEST + ".hs";
        ProcessLauncher launcher = new ProcessLauncher(
            true,
            new File(ghcBin.getPath(), "ghc").getAbsolutePath(),
            "--make", "-package", "ghc",
            "-i" + pluginPath.getAbsolutePath(),
            new File(pluginPath, mainHs).getAbsolutePath()
        );
        for (int i = 0; i < 3; i++) {
            if (exe.exists())
                return true;
            Thread.sleep(100);
        }
        String stdErr = launcher.getStdErr();
        LOG.error("Compiling " + mainHs + ":\n" + stdErr);
        return false;
    }
}
