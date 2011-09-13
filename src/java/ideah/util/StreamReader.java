package ideah.util;

import com.intellij.openapi.diagnostic.Logger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

final class StreamReader extends Thread {

    private static final Logger LOG = Logger.getInstance("ideah.util.StreamReader");

    private final InputStream stream;
    private final StringBuilder buf = new StringBuilder();

    StreamReader(InputStream stream) {
        this.stream = stream;
    }

    public synchronized void run() {
        try {
            BufferedReader rdr = new BufferedReader(new InputStreamReader(stream, "UTF8"));
            while (true) {
                int c = rdr.read();
                if (c < 0)
                    break;
                buf.append((char) c);
            }
            rdr.close();
        } catch (IOException e) {
            LOG.error(e);
        }
    }

    synchronized String getOutput() {
        return buf.toString();
    }
}
