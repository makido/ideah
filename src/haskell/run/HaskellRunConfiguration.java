package haskell.run;

import com.intellij.execution.CommonProgramRunConfigurationParameters;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.ExternalizablePath;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.configurations.RuntimeConfiguration;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import haskell.parser.HaskellFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;

public final class HaskellRunConfiguration extends RuntimeConfiguration implements CommonProgramRunConfigurationParameters {

    public HaskellFile MAIN_FILE;
    public String RT_FLAGS;
    public String PROGRAM_PARAMETERS;
    public String WORKING_DIRECTORY;
    public boolean PASS_PARENT_ENVS = true;

    private Map<String, String> myEnvs = new LinkedHashMap<String, String>();

    public HaskellRunConfiguration(Project project, ConfigurationFactory factory) {
        super("Haskell", project, factory);
    }

    public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
        return new ConfigurationEditor(getProject());
    }

    public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
        return null; // todo
    }

    public void setProgramParameters(@Nullable String value) {
        PROGRAM_PARAMETERS = value;
    }

    public String getProgramParameters() {
        return PROGRAM_PARAMETERS;
    }

    public void setWorkingDirectory(@Nullable String value) {
        WORKING_DIRECTORY = ExternalizablePath.urlValue(value);
    }

    public String getWorkingDirectory() {
        return ExternalizablePath.localPathValue(WORKING_DIRECTORY);
    }

    public void setEnvs(@NotNull Map<String, String> envs) {
        this.myEnvs = envs;
    }

    @NotNull
    public Map<String, String> getEnvs() {
        return myEnvs;
    }

    public void setPassParentEnvs(boolean passParentEnvs) {
        PASS_PARENT_ENVS = passParentEnvs;
    }

    public boolean isPassParentEnvs() {
        return PASS_PARENT_ENVS;
    }

    @Override
    public String suggestedName() {
        return MAIN_FILE == null ? "Unnamed" : MAIN_FILE.getName();
    }

    // todo: read/write external???
}
