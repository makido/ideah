package haskell.compiler;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import com.intellij.openapi.diagnostic.Logger;


final class GHCMessageReader extends Thread {

    private final InputStream stream;
    private final List<GHCMessage> ghcMessages = new ArrayList<GHCMessage>();

    private static final Logger LOG = Logger.getInstance("#org.jetbrains.plugins.clojure.compiler.ClojureBackendCompiler");

    protected GHCMessageReader(InputStream stream) {
        this.stream = stream;
    }

    public synchronized void run() {
        BufferedReader ghcErrorReader = null;
        try {
            ghcErrorReader = new BufferedReader(new InputStreamReader(stream, "UTF8"));
        } catch (UnsupportedEncodingException e) {
            LOG.error(e);
        }
        List<StringBuffer> buffers = new ArrayList<StringBuffer>();
        try {

            String newLine = System.getProperty("line.separator");
            StringBuffer tmpBuffer = new StringBuffer();
            String line = ghcErrorReader.readLine();
            while (line != null) {
                if (line.startsWith("\f")) {
                    tmpBuffer = new StringBuffer();
                    buffers.add(tmpBuffer);
                } else {
                    tmpBuffer.append(line).append(newLine);
                }
                line = ghcErrorReader.readLine();
            }
            for (StringBuffer buffer : buffers) {
                ghcMessages.add(new GHCMessage(buffer.toString()));
            }
        } catch (IOException e) {
            LOG.error(e);
        }
    }

    protected synchronized List<GHCMessage> getGHCErrors() {
        return ghcMessages;
    }
}
