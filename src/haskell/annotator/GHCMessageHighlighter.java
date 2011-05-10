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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

public final class GHCMessageHighlighter implements ExternalAnnotator {

    private static final Logger LOG = Logger.getInstance("haskell.annotator.GHCMessageHighlighter");

    public void annotate(PsiFile psiFile, AnnotationHolder annotationHolder) {
        VirtualFile file = psiFile.getVirtualFile();
        try {
            Project project = psiFile.getProject();
            ProjectRootManager rootManager = ProjectRootManager.getInstance(project);
            Module module = rootManager.getFileIndex().getModuleForFile(file);
            VirtualFile ghcLib = LaunchGHC.getLibPath(module);
            List<GHCMessage> ghcMessages = LaunchGHC.getGHCMessages(ghcLib, null, file);
            for (GHCMessage ghcMessage : ghcMessages) {
                TextRange range = new TextRange(
                    getPos(file, ghcMessage.getStartLine(), ghcMessage.getEndColumn()),
                    getPos(file, ghcMessage.getEndLine(), ghcMessage.getEndColumn())
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
        } catch (IOException e) {
            LOG.error(e);
        }
    }

    // todo: something wrong here, if error at the very end of file
    private static int getPos(VirtualFile file, int line, int column) throws IOException {
        InputStream is = file.getInputStream();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(is, file.getCharset()));
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
        } finally {
            is.close();
        }
    }
}
