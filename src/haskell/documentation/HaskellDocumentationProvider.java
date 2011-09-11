package haskell.documentation;

import com.intellij.lang.documentation.DocumentationProvider;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.LazyRangeMarkerFactory;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import haskell.psi.api.HPIdent;
import haskell.util.CompilerLocation;
import haskell.util.ProcessLauncher;

import java.io.IOException;
import java.util.List;

public final class HaskellDocumentationProvider implements DocumentationProvider {

    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        return null;
    }

    public List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        return null; // todo: find external docs
    }

    public String generateDoc(PsiElement element, PsiElement originalElement) {
        if (element instanceof HPIdent) {
            HPIdent ident = (HPIdent) element;
            TextRange range = ident.getTextRange();
            PsiFile psiFile = element.getContainingFile();
            VirtualFile file = psiFile.getVirtualFile();
            if (file == null)
                return null;
            FileDocumentManager fdm = FileDocumentManager.getInstance();
            Document doc = fdm.getCachedDocument(file);
            if (doc == null)
                return null;
            int offset = range.getStartOffset();
            int line = doc.getLineNumber(offset);
            int col = offset - doc.getLineStartOffset(line);
            System.out.println((line + 1) + ":" + (col + 1));
            Module module = ProjectRootManager.getInstance(psiFile.getProject()).getFileIndex().getModuleForFile(file);
            CompilerLocation compiler = CompilerLocation.get(module);
            if (compiler == null) {
                return null;
            }
            try {
                ProcessLauncher launcher = new ProcessLauncher(
                    false,
                    compiler.exe,
                    "-m", "GetIdType",
                    "-g", compiler.libPath,
                    "-s", CompilerLocation.rootsToString(ModuleRootManager.getInstance(module).getSourceRoots(false)),
                    "-ln", String.valueOf(line + 1), "-col", String.valueOf(col + 1),
                    file.getPath()
                );
                return launcher.getStdOut();
            } catch (Exception ex) {
                // todo: log
                return null;
            }
        }
        return null;
    }

    public PsiElement getDocumentationElementForLookupItem(PsiManager psiManager, Object object, PsiElement element) {
        return null;
    }

    public PsiElement getDocumentationElementForLink(PsiManager psiManager, String link, PsiElement context) {
        return null;
    }
}
