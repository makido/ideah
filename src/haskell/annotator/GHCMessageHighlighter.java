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

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
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
            List<GHCMessage> ghcMessages = LaunchGHC.getGHCMessages(ghcLib, null, file.getPath(), module, true);
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
        Charset charset = file.getCharset();
        FileChannel channel = new FileInputStream(file.getPath()).getChannel();
        ByteBuffer byteBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, (int)channel.size());
        CharBuffer charBuffer = Charset.forName(charset.name()).newDecoder().decode(byteBuffer);
        String fileContent = charBuffer.toString();

        InputStream is = file.getInputStream();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(is, charset));
            int pos = 0;
            int newlineLength = 0;
            for (int i = 0; i < line; i++) {
                String currentLine = reader.readLine();
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
                        if (currentLine.length() != 0) {
                            fileContent = fileContent.substring(currentLine.length() + newlineLength);
                            newlineLength = getNewlineLength(fileContent);
                            pos += newlineLength;
                        }
                    }
                } else {
                    LOG.error("Error position out of bounds: line " + line);
                }
            }
            return pos;
        } finally {
            is.close();
        }
    }

    private static int getNewlineLength(String str) {
        return str.split(".", 4)[0].length();
    }
}
