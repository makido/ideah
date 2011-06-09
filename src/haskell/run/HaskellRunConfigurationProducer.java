package haskell.run;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import haskell.parser.HaskellFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
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
                    configuration.MAIN_FILE = runFile;
                    // todo: set working dir
                    configuration.setName(configuration.getGeneratedName());
                    return settings;
                } else {
                    LOG.error("No main function in module " + hsFile.getName());
                    return null;
                }
            }
        }
        return null;
    }

    private static boolean hasMain(String haskellCode) {
        BufferedReader reader = new BufferedReader(new StringReader(haskellCode));
        try {
            String line = reader.readLine();
            while (line != null && (isEmpty(line) || line.startsWith("module "))) {      // it cannot start with 'module' more than once
                line = reader.readLine();
            }
            if (line != null) {
                int indent = 0;
                String indentLine = line;
                while (Character.isSpaceChar(indentLine.charAt(0))) {
                    indent++;
                    indentLine = indentLine.substring(1);
                }
                String main = "main";
                int mainLength = main.length();
                int minLength = mainLength + indent;
                while (line != null) {
                    if (line.length() >= minLength) {
                        line = line.substring(indent);
                        if (line.startsWith(main)) {
                            String trim = line.substring(mainLength).trim();
                            if (trim.isEmpty()) {
                                while (line != null && isEmpty(line)) {
                                    line = reader.readLine();
                                }
                                if (line != null && line.substring(indent + 1).trim().charAt(0) == '=') {
                                    return true;
                                }
                            } else {
                                return trim.charAt(0) == '=';
                            }
                        }
                    }
                    line = reader.readLine();
                }
            }
        } catch (IOException e) {
            return false;
        }
        return false;
    }

    private static boolean isEmpty(String string) {
        return string.trim().isEmpty();
    }

    public int compareTo(Object o) {
        return PREFERED;
    }
}
