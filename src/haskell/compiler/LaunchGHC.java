package haskell.compiler;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;

import java.util.*;

public final class LaunchGHC {

    private static final Logger LOG = Logger.getInstance("haskell.compiler.LaunchGHC");

    public static List<GHCMessage> getGHCMessages(VirtualFile libPath, VirtualFile output, String fileName,
                                                  Module module, boolean tests) {
        Runtime runtime = Runtime.getRuntime();
        try {
            List<String> args = new ArrayList<String>();
            String exe = "E:\\Dropbox\\Private\\Ideah\\project\\haskell\\err_test.exe";
//            String exe = "D:\\home\\oleg\\haskell\\idea\\haskell\\haskell\\err_test.exe";
            args.add(exe);
            Set<VirtualFile> sourceRoots = new HashSet<VirtualFile>();
            sourceRoots.addAll(Arrays.asList(ModuleRootManager.getInstance(module).getSourceRoots(tests)));
            for (VirtualFile sourceRoot : sourceRoots) {
                VirtualFile[] children = sourceRoot.getChildren();
                for (VirtualFile child : children) {
                    if (child.isDirectory()) {
                        sourceRoots.add(child);
                    }
                }
            }
            args.addAll(Arrays.asList(
                "-g", libPath.getPath(),
                "-c", "-W",
                "-s", rootsToString(sourceRoots)
            ));
            if (output != null) {
                args.addAll(Arrays.asList(
                    "-o", output.getPath()
                ));
            }
            args.add(fileName);
            Process process = runtime.exec(args.toArray(new String[args.size()]));
            GHCMessageReader stdin = new GHCMessageReader(process.getInputStream());
            GHCMessageReader stderr = new GHCMessageReader(process.getErrorStream());
            stdin.start();
            stderr.start();
            stdin.join();
            return stdin.getGHCMessages();
        } catch (Exception ex) {
            LOG.error(ex);
            return Collections.singletonList(new GHCMessage(ex.toString(), fileName));
        }
    }

    private static String rootsToString(Collection<VirtualFile> roots) {
        StringBuilder sourceRoots = new StringBuilder();
        for (VirtualFile root : roots) {
            sourceRoots.append(":").append(root.getPath());
        }
        return sourceRoots.toString().substring(1);
    }

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
