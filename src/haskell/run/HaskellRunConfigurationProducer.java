package haskell.run;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import haskell.parser.HaskellFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

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
                if (hasMain(hsFile.getText())) {
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
            }
        }
        return null;
    }

    private static boolean hasMain(String haskellCode) {

        return false;
    }

    private static boolean isEmpty(String string) {
        return string.trim().isEmpty();
    }

    public int compareTo(Object o) {
        return PREFERED;
    }
}
