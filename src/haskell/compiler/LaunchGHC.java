package haskell.compiler;

import java.io.IOException;
import java.util.List;

// todo: make callable from IDEA
public class LaunchGHC {

    public static void main(String[] args) {
        Runtime runtime = Runtime.getRuntime();
        try {
            /*int argsLength = args.length;
            String[] cmdArray = new String[argsLength + 1];
            cmdArray[0] = "./err_test.hs";
            System.arraycopy(args, 0, cmdArray, 1, argsLength);
            Process process = runtime.exec(cmdArray);*/
            //args = new String[] {"test.hs"};
            Process process = runtime.exec("./err_test.exe " + args[0]);
            GHCMessageReader stdin = new GHCMessageReader(process.getInputStream());
            GHCMessageReader stderr = new GHCMessageReader(process.getErrorStream());
            stdin.start();
            stderr.start();
            stdin.join();
            stderr.join();
            // todo: only 1 stream
            markErrors(stdin.getGHCErrors());
            markErrors(stderr.getGHCErrors());
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public static void markErrors(List<GHCError> errorMarker) {
        // todo
    }
}
