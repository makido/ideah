package haskell.compiler;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

// todo: make unpublic final
public class GHCMessageReader extends Thread {

    private InputStream stream; // todo: make final
    private List<GHCError> ghcErrors = new ArrayList<GHCError>(); // todo: make final

    GHCMessageReader(InputStream stream) {
        this.stream = stream;
    }

    // todo: remove synchronized
    public synchronized void run() {
        BufferedReader ghcErrorReader = new BufferedReader(new InputStreamReader(stream)); // todo: read in UTF8?
        List<StringBuffer> buffers = new ArrayList<StringBuffer>();
        try {
            final String newLine = System.getProperty("line.separator"); // todo: remove final
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
                ghcErrors.add(new GHCError(buffer.toString()));
            }
        } catch (IOException e) {
            e.printStackTrace(); // todo: use IDEA logger?
        }
    }

    // todo: remove public synchronized
    public synchronized List<GHCError> getGHCErrors() {
        return ghcErrors;
    }
}
