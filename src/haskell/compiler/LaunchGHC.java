package haskell.compiler;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import haskell.util.ProcessLauncher;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;

public final class LaunchGHC {

    private static final Logger LOG = Logger.getInstance("haskell.compiler.LaunchGHC");

    static final String EOLN = "\n";

    public static List<GHCMessage> getGHCMessages(VirtualFile output, String fileName, Module module, boolean tests) {
        try {
            List<String> args = new ArrayList<String>();
//            String exe = "E:\\Dropbox\\Private\\Ideah\\project\\haskell\\err_test.exe";
            String exe = "D:\\home\\oleg\\haskell\\idea\\haskell\\haskell\\err_test.exe";
            args.add(exe);
            VirtualFile[] sourceRoots = ModuleRootManager.getInstance(module).getSourceRoots(tests);
            args.addAll(Arrays.asList(
                "-g", getLibPath(module).getPath(),
                "-c", "-W",
                "-s", rootsToString(sourceRoots)
            ));
            if (output != null) {
                args.addAll(Arrays.asList(
                    "-o", output.getPath()
                ));
            }
            args.add(fileName);
            ProcessLauncher launcher = new ProcessLauncher(args);
            String stdOut = launcher.getStdOut();
            return parseMessages(stdOut);
        } catch (Exception ex) {
            LOG.error(ex);
            return Collections.singletonList(new GHCMessage(ex.toString(), fileName));
        }
    }

    private static String rootsToString(VirtualFile[] roots) {
        StringBuilder sourceRoots = new StringBuilder();
        for (VirtualFile root : roots) {
            sourceRoots.append(":").append(root.getPath());
        }
        return sourceRoots.substring(1);
    }

    private static List<GHCMessage> parseMessages(String output) throws IOException {
        List<StringBuffer> buffers = new ArrayList<StringBuffer>();
        List<GHCMessage> ghcMessages = new ArrayList<GHCMessage>();
        BufferedReader ghcErrorReader = new BufferedReader(new StringReader(output));
        StringBuffer tmpBuffer = new StringBuffer();
        String line = ghcErrorReader.readLine();
        while (line != null) {
            if (line.startsWith("\f")) {
                tmpBuffer = new StringBuffer();
                buffers.add(tmpBuffer);
            } else {
                tmpBuffer.append(line).append(EOLN);
            }
            line = ghcErrorReader.readLine();
        }
        for (StringBuffer buffer : buffers) {
            ghcMessages.add(new GHCMessage(buffer.toString()));
        }
        return ghcMessages;
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
