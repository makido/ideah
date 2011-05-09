package haskell.compiler;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import com.intellij.openapi.diagnostic.Logger;

final class GHCMessageReader extends Thread {

    static final String EOLN = "\n";
    private static final Logger LOG = Logger.getInstance("haskell.compiler.GHCMessageReader");

    private final InputStream stream;
    private final List<GHCMessage> ghcMessages = new ArrayList<GHCMessage>();

    GHCMessageReader(InputStream stream) {
        this.stream = stream;
    }

    public synchronized void run() {
        List<StringBuffer> buffers = new ArrayList<StringBuffer>();
        try {
            BufferedReader ghcErrorReader = new BufferedReader(new InputStreamReader(stream, "UTF8"));
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
        } catch (IOException e) {
            LOG.error(e);
        }
    }

    synchronized List<GHCMessage> getGHCMessages() {
        return ghcMessages;
    }
}
