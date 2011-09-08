package haskell.run;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import haskell.compiler.LaunchGHC;
import haskell.parser.HaskellFile;
import haskell.util.FileNames;
import haskell.util.Paths;
import haskell.util.ProcessLauncher;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
                        System.out.println("main found");
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
                } catch (IOException e) {
                    LOG.error(e);
                } catch (InterruptedException e) {
                    LOG.error(e);
                }
            }
        }
        return null;
    }

    private static boolean hasMain(String filePath, Module module) throws IOException, InterruptedException {
        String exe = LaunchGHC.getErrTestExe(module);
        if (exe == null) {
            return false;
        }
        List<String> args = new ArrayList<String>();
        args.addAll(Arrays.asList(FileNames.getFullErrTestExeName(),
                "-m", "-g", Paths.getLibVFile(module).getPath(),
                filePath));
        ProcessLauncher launcher = new ProcessLauncher(false, args);
        String stdErr = launcher.getStdErr();
        BufferedReader reader = new BufferedReader(new StringReader(stdErr));
        String line = reader.readLine();
        while (line != null) {
            if (line.startsWith("\f")) {
                reader.close();
                return line.contains("t");
            }
            line = reader.readLine();
        }
        reader.close();
        return false;
    }

    private static boolean isEmpty(String string) {
        return string.trim().isEmpty();
    }

    public int compareTo(Object o) {
        return PREFERED;
    }
}
