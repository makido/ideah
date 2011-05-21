package haskell.annotator;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import haskell.compiler.GHCMessage;
import haskell.compiler.LaunchGHC;

import java.util.List;

public final class GHCMessageHighlighter implements ExternalAnnotator {

    private static final Logger LOG = Logger.getInstance("haskell.annotator.GHCMessageHighlighter");

    public void annotate(PsiFile psiFile, AnnotationHolder annotationHolder) {
        VirtualFile file = psiFile.getVirtualFile();
        String text = psiFile.getText();
        Project project = psiFile.getProject();
        ProjectRootManager rootManager = ProjectRootManager.getInstance(project);
        Module module = rootManager.getFileIndex().getModuleForFile(file);
        VirtualFile ghcLib = LaunchGHC.getLibPath(module);
        List<GHCMessage> ghcMessages = LaunchGHC.getGHCMessages(ghcLib, null, file.getPath(), module, true);
        for (GHCMessage ghcMessage : ghcMessages) {
            TextRange range = new TextRange(
                getPos(text, ghcMessage.getStartLine(), ghcMessage.getEndColumn()),
                getPos(text, ghcMessage.getEndLine(), ghcMessage.getEndColumn())
            );
            String message = ghcMessage.getErrorMessage();
            CompilerMessageCategory category = ghcMessage.getCategory();
            switch (category) {
            case ERROR:
                annotationHolder.createErrorAnnotation(range, message);
                break;
            case WARNING:
                annotationHolder.createWarningAnnotation(range, message);
                break;
            case INFORMATION:
                annotationHolder.createInfoAnnotation(range, message);
                break;
            case STATISTICS:
                break;
            }
        }
    }

    private static int getPos(String text, int line, int column) {
        int pos = 0;
        int linesLeft = line - 1;
        while (linesLeft >= 0) {
            int newLineLength = getNewLineLength(text);
            if (linesLeft == 0) {
                int newLineIndex = getNewLineIndex(text);
                if (column >= (newLineIndex >= 0 ? text.substring(0, newLineIndex) : text).length()) {
                    pos += column;
                } else {
                    outOfBoundsError(line, column);
                }
                break;
            } else {
                if (text.length() > 0) {
                    if (newLineLength > 0) {
                        text = text.substring(newLineLength);
                        pos += newLineLength;
                        linesLeft--;
                    } else {
                        text = text.substring(1);
                        pos++;
                    }
                } else {
                    outOfBoundsError(line, column);
                }
            }
        }
        return pos - 1;
    }

    private static void outOfBoundsError(int line, int column) {
        LOG.error("Error position out of bounds: line " + line + ", column " + column);
    }

    private static int getNewLineLength(String text) {
        String linuxnl = "\r\n";
        char winnl = '\n';
        char macnl = '\r';
        if (text.startsWith(linuxnl)) {
            return linuxnl.length();
        }
        char c = text.charAt(0);
        if (c == winnl || c == macnl) {
            return 1;
        }
        return 0;
    }

    private static int getNewLineIndex(String text) {
        int i = text.indexOf('\n');
        if (i < 0) {
            i = text.indexOf('\r');
        }
        return i;
    }
}
