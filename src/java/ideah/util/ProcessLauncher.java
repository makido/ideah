package ideah.util;

import java.io.IOException;
import java.util.List;

public final class ProcessLauncher {

    private final String stdOut;
    private final String stdErr;

    public ProcessLauncher(boolean waitFor, List<String> args) throws InterruptedException, IOException {
        this(waitFor, args.toArray(new String[args.size()]));
    }

    public ProcessLauncher(boolean waitFor, String... args) throws InterruptedException, IOException {
        Process process = Runtime.getRuntime().exec(args);
        StreamReader outReader = new StreamReader(process.getInputStream());
        StreamReader errReader = new StreamReader(process.getErrorStream());
        outReader.start();
        errReader.start();
        errReader.join();
        outReader.join();
        this.stdOut = outReader.getOutput();
        this.stdErr = errReader.getOutput();
        if (waitFor) {
            process.waitFor();
        }
    }

    public String getStdOut() {
        return stdOut;
    }

    public String getStdErr() {
        return stdErr;
    }
}
