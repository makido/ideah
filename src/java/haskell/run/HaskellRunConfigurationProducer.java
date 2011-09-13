package haskell.run;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import haskell.compiler.LaunchGHC;
import haskell.parser.HaskellFile;
import haskell.util.CompilerLocation;
import haskell.util.ProcessLauncher;

import java.io.File;
import java.io.IOException;

public final class HaskellRunConfigurationProducer extends RuntimeConfigurationProducer {

    private HaskellFile runFile;

    private static final Logger LOG = Logger.getInstance("haskell.run.HaskellRunConfigurationProducer");

    public HaskellRunConfigurationProducer() {
        super(HaskellRunConfigurationType.INSTANCE);
    }

    public PsiElement getSourceElement() {
        return runFile;
    }

    protected RunnerAndConfigurationSettings createConfigurationByElement(Location location, ConfigurationContext context) {
        PsiFile file = location.getPsiElement().getContainingFile();

        if (file instanceof HaskellFile) {
            HaskellFile hsFile = (HaskellFile) file;
            if (hsFile.isMainModule()) {
                try {
                    if (hasMain(file.getVirtualFile().getPath(), context.getModule())) {
                        runFile = hsFile;
                        Project project = file.getProject();
                        RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(project, context);
                        HaskellRunConfiguration configuration = (HaskellRunConfiguration) settings.getConfiguration();
                        configuration.setMainFile(runFile);
                        VirtualFile baseDir = project.getBaseDir();
                        if (baseDir != null) {
                            configuration.setWorkingDirectory(baseDir.getPath());
                        }
                        configuration.setName(configuration.getGeneratedName());
                        return settings;
                    } else {
                        return null;
                    }
                } catch (Exception ex) {
                    LOG.error(ex);
                }
            }
        }
        return null;
    }

    private static boolean hasMain(String filePath, Module module) throws IOException, InterruptedException {
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null) {
            return false;
        }
        ProcessLauncher launcher = new ProcessLauncher(
            false,
            compiler.exe,
            "-m", "CheckMain",
            "-g", compiler.libPath,
            "-s", CompilerLocation.rootsToString(ModuleRootManager.getInstance(module).getSourceRoots(false)),
            filePath
        );
        String stdOut = launcher.getStdOut();
        return stdOut != null && stdOut.contains("t");
    }

    public int compareTo(Object o) {
        return PREFERED;
    }
}
