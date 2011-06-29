package haskell.run;

import com.intellij.execution.CommonProgramRunConfigurationParameters;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.*;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.JDOMExternalizer;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import haskell.parser.HaskellFile;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public final class HaskellRunConfiguration extends ModuleBasedConfiguration<RunConfigurationModule> implements CommonProgramRunConfigurationParameters {

    private String mainFile;
    private String rtFlags;
    private String programParameters;
    private String workingDir;
    private boolean passParentEnvs = true;

    private final Map<String, String> myEnvs = new LinkedHashMap<String, String>();

    public HaskellRunConfiguration(String name, Project project, ConfigurationFactory factory) {
        super(name, new RunConfigurationModule(project), factory);
    }

    public HaskellRunConfiguration(Project project, ConfigurationFactory factory) {
        this("Haskell", project, factory);
    }

    @Nullable
    public Module getModule() {
        return getConfigurationModule().getModule();
    }

    public Collection<Module> getValidModules() {
        Module[] modules = ModuleManager.getInstance(getProject()).getModules();
        return Arrays.asList(modules);
    }

    @Override
    protected HaskellRunConfiguration createInstance() {
        return new HaskellRunConfiguration(getName(), getProject(), getFactory());
    }

    public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
        return new ConfigurationEditor(getProject());
    }

    public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) {
        HaskellCommandLineState state = new HaskellCommandLineState(env, this);
        state.setConsoleBuilder(TextConsoleBuilderFactory.getInstance().createBuilder(getProject()));
        return state;
    }

    @Override
    public void checkConfiguration() throws RuntimeConfigurationException {
        super.checkConfiguration();
        // todo: check if mainFile exists?
        // todo: check if still has main function
    }

    // getters/setters

    public void setProgramParameters(@Nullable String value) {
        programParameters = value;
    }

    public String getProgramParameters() {
        return programParameters;
    }

    public void setWorkingDirectory(@Nullable String value) {
        workingDir = value;
    }

    public String getWorkingDirectory() {
        return workingDir;
    }

    public void setEnvs(@NotNull Map<String, String> envs) {
        this.myEnvs.clear();
        this.myEnvs.putAll(envs);
    }

    @NotNull
    public Map<String, String> getEnvs() {
        return myEnvs;
    }

    public void setPassParentEnvs(boolean passParentEnvs) {
        this.passParentEnvs = passParentEnvs;
    }

    public boolean isPassParentEnvs() {
        return passParentEnvs;
    }

    public String getMainFile() {
        return mainFile;
    }

    public void setMainFile(HaskellFile mainFile) {
        VirtualFile file = mainFile.getVirtualFile();
        if (file != null) {
            this.mainFile = file.getPath();
            Module module = ProjectRootManager.getInstance(mainFile.getProject()).getFileIndex().getModuleForFile(file);
            setModule(module);
        }
    }

    public String getRuntimeFlags() {
        return rtFlags;
    }

    public void setRuntimeFlags(String rtFlags) {
        this.rtFlags = rtFlags;
    }

    // end of getters/setters

    @Override
    public String suggestedName() {
        VirtualFile file;
        if (mainFile == null) {
            file = null;
        } else {
            file = LocalFileSystem.getInstance().findFileByPath(mainFile);
        }
        if (file == null) {
            return "Unnamed";
        } else {
            return file.getName();
        }
    }

    public void readExternal(Element element) throws InvalidDataException {
        PathMacroManager.getInstance(getProject()).expandPaths(element);
        super.readExternal(element);
        readModule(element);
        mainFile = JDOMExternalizer.readString(element, "mainFile");
        programParameters = JDOMExternalizer.readString(element, "params");
        rtFlags = JDOMExternalizer.readString(element, "rtFlags");
        String wrk = JDOMExternalizer.readString(element, "workDir");
        if (!".".equals(wrk)) {
            workingDir = wrk;
        }
        myEnvs.clear();
        JDOMExternalizer.readMap(element, myEnvs, null, "env");
        passParentEnvs = JDOMExternalizer.readBoolean(element, "passParentEnv");
    }

    public void writeExternal(Element element) throws WriteExternalException {
        super.writeExternal(element);
        writeModule(element);
        JDOMExternalizer.write(element, "mainFile", mainFile);
        JDOMExternalizer.write(element, "params", programParameters);
        JDOMExternalizer.write(element, "rtFlags", rtFlags);
        JDOMExternalizer.write(element, "workDir", workingDir);
        JDOMExternalizer.writeMap(element, myEnvs, null, "env");
        JDOMExternalizer.write(element, "passParentEnv", passParentEnvs);
        PathMacroManager.getInstance(getProject()).collapsePathsRecursively(element);
    }
}
