package ideah.compiler;

import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import ideah.HaskellFileType;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.HashSet;

public final class HaskellCompilerProjectComponent implements ProjectComponent {

    private final Project project;

    public HaskellCompilerProjectComponent(Project project) {
        this.project = project;
    }

    public void projectOpened() {
        CompilerManager manager = CompilerManager.getInstance(project);
        for (HaskellCompiler compiler : manager.getCompilers(HaskellCompiler.class)) {
            manager.removeCompiler(compiler);
        }
        HashSet<FileType> inputSet = new HashSet<FileType>(Arrays.asList(HaskellFileType.INSTANCE));
        HashSet<FileType> outputSet = new HashSet<FileType>(Arrays.asList(HiFileType.INSTANCE));
        manager.addTranslatingCompiler(new HaskellCompiler(project), inputSet, outputSet);
    }

    public void projectClosed() {
    }

    @NotNull
    public String getComponentName() {
        return "HaskellCompilerComponent";
    }

    public void initComponent() {
    }

    public void disposeComponent() {
    }
}
