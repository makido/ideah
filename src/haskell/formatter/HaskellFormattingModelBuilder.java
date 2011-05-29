package haskell.formatter;

import com.intellij.formatting.Block;
import com.intellij.formatting.FormattingModel;
import com.intellij.formatting.FormattingModelBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.formatter.DocumentBasedFormattingModel;
import haskell.util.LineColRange;
import haskell.util.ProcessLauncher;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class HaskellFormattingModelBuilder implements FormattingModelBuilder {

    private static final Logger LOG = Logger.getInstance("haskell.formatter.HaskellFormattingModelBuilder");

    @NotNull
    public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
        PsiFile file = element.getContainingFile();
        Project project = file.getProject();
        VirtualFile virtualFile = file.getVirtualFile();
        List<Block> functions = new ArrayList<Block>();
        if (virtualFile != null) {
            try {
                String path = virtualFile.getPath();
                String output = new ProcessLauncher("D:\\home\\oleg\\haskell\\idea\\haskell\\haskell\\format.exe", path).getStdOut();
                BufferedReader rdr = new BufferedReader(new StringReader(output));
                while (true) {
                    String line = rdr.readLine();
                    if (line == null)
                        break;
                    LineColRange range = new LineColRange(line);
                    String str = range.getRange(file).substring(file.getText());
                    System.out.println(range + "> '" + str + "'");
                    functions.add(new HaskellBlock(range.getRange(file), Collections.<Block>emptyList()));
                }
            } catch (Exception ex) {
                LOG.error(ex);
            }
        }
        Block rootBlock = new HaskellBlock(file.getTextRange(), functions);
        return new DocumentBasedFormattingModel(rootBlock, project, settings, file.getFileType(), file);
    }

    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
