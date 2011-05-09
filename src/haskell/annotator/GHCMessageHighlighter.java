package haskell.annotator;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import haskell.compiler.GHCMessage;
import haskell.compiler.LaunchGHC;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

public final class GHCMessageHighlighter implements ExternalAnnotator {

    private static final Logger LOG = Logger.getInstance("haskell.compiler.LaunchGHC");

    public void annotate(PsiFile psiFile, AnnotationHolder annotationHolder) {
        VirtualFile file = psiFile.getVirtualFile();
        try {
            List<GHCMessage> ghcMessages = LaunchGHC.getGHCMessages(file.getPath());
            int warningLength = "Warning: ".length();
            for (GHCMessage ghcMessage : ghcMessages) {
                TextRange range = new TextRange(
                    getPos(file, ghcMessage.getStartLine(), ghcMessage.getEndColumn()),
                    getPos(file, ghcMessage.getEndLine(), ghcMessage.getEndColumn()));
                String message = ghcMessage.getErrorMessage();
                if (ghcMessage.isWarning()) {
                    annotationHolder.createWarningAnnotation(range, message.substring(warningLength));
                } else {
                    annotationHolder.createErrorAnnotation(range, message);
                }
            }
        } catch (NullPointerException e) {
            LOG.error(e);
        } catch (IOException e) {
            LOG.error(e);
        }
    }

    private static int getPos(VirtualFile file, int line, int column) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream()));
        String currentLine = reader.readLine();
        int pos = -2;
        for (int i = 0; i < line; i++) {
            pos += 2;
            if (currentLine != null) {
                int lineLength = currentLine.length();
                if (i == line - 1) {
                    if (column <= lineLength) {
                        pos += column - 1;
                    } else {
                        LOG.error("Error position out of bounds: line " + line + ", column " + column);
                    }
                } else {
                    pos += lineLength - 1;
                }
            } else {
                LOG.error("Error position out of bounds: line " + line);
            }
            currentLine = reader.readLine();
        }
        return pos;
    }
}
