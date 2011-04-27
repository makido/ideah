package haskell.compiler;

import com.intellij.openapi.diagnostic.Logger;

import java.io.*;
import java.util.List;

public class LaunchGHC {

    private static final Logger LOG = Logger.getInstance("#org.jetbrains.plugins.clojure.compiler.ClojureBackendCompiler");

    public static void main(String[] args) {
        Runtime runtime = Runtime.getRuntime();
        try {
            /*int argsLength = args.length;
            String[] cmdArray = new String[argsLength + 1];
            cmdArray[0] = "./err_test.hs";
            System.arraycopy(args, 0, cmdArray, 1, argsLength);
            Process process = runtime.exec(cmdArray);*/
            args = new String[]{"test.hs"};
            Process process = runtime.exec("./err_test.exe " + args[0]);
            GHCMessageReader stdin = new GHCMessageReader(process.getInputStream());
            GHCMessageReader stderr = new GHCMessageReader(process.getErrorStream());
            stdin.start();
            stderr.start();
            stdin.join();
            stderr.join();
            markErrors(stdin.getGHCErrors());
            markErrors(stderr.getGHCErrors());
        } catch (IOException e) {
            LOG.error(e);
        } catch (InterruptedException e) {
            LOG.error(e);
        }
    }

    public static void markErrors(List<GHCMessage> messageMarker) {
        // todo
    }
}
