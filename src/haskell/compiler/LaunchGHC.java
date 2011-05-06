package haskell.compiler;

import com.intellij.openapi.diagnostic.Logger;

import java.io.*;
import java.util.List;

public class LaunchGHC {

    private static final Logger LOG = Logger.getInstance("haskell.compiler.LaunchGHC");

    public static List<GHCMessage> getGHCMessages(String fileName) {
        Runtime runtime = Runtime.getRuntime();
        try {
            Process process = runtime.exec(
                    "E:\\Dropbox\\Private\\Ideah\\project\\haskell\\err_test.exe " +
                            "-g \"C:/Program Files (x86)/Haskell Platform/2010.2.0.0/lib/\" " +
                            "-c \"-W\" " +
                            fileName);
            GHCMessageReader stdin = new GHCMessageReader(process.getInputStream());
            GHCMessageReader stderr = new GHCMessageReader(process.getErrorStream());
            stdin.start();
            stderr.start();
            stdin.join();
            stderr.join();
            List<GHCMessage> returnList = stdin.getGHCMessages();
            returnList.addAll(stderr.getGHCMessages());
            return returnList;
        } catch (IOException e) {
            LOG.error(e);
        } catch (InterruptedException e) {
            LOG.error(e);
        }
        return null;
    }
}
