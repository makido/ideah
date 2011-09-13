package ideah.run;

import com.intellij.execution.configuration.ConfigurationFactoryEx;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.project.Project;
import ideah.HaskellFileType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public final class HaskellRunConfigurationType implements ConfigurationType {

    public static final HaskellRunConfigurationType INSTANCE = new HaskellRunConfigurationType();

    private final ConfigurationFactory myFactory;

    public HaskellRunConfigurationType() {
        this.myFactory = new ConfigurationFactoryEx(this) {
            public RunConfiguration createTemplateConfiguration(Project project) {
                return new HaskellRunConfiguration(project, this);
            }
        };
    }

    public String getDisplayName() {
        return "Haskell";
    }

    public String getConfigurationTypeDescription() {
        return "Haskell application";
    }

    public Icon getIcon() {
        return HaskellFileType.HASKELL_ICON; // todo: other icon?
    }

    @NotNull
    public String getId() {
        return "HaskellRunConfiguration";
    }

    public ConfigurationFactory[] getConfigurationFactories() {
        return new ConfigurationFactory[] {myFactory};
    }
}
