package haskell.run;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import haskell.parser.HaskellFile;

public final class HaskellRunConfigurationProducer extends RuntimeConfigurationProducer {

    private HaskellFile runFile;

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
                // todo: check if has main function
                runFile = hsFile;
                Project project = file.getProject();
                RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(project, context);
                HaskellRunConfiguration configuration = (HaskellRunConfiguration) settings.getConfiguration();
                configuration.MAIN_FILE = runFile;
                // todo: set working dir
                configuration.setName(configuration.getGeneratedName());
                return settings;
            }
        }
        return null;
    }

    public int compareTo(Object o) {
        return PREFERED;
    }
}
