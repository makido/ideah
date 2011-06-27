package haskell.run;

import com.intellij.execution.*;
import com.intellij.execution.configurations.*;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import haskell.compiler.HaskellSdkType;
import haskell.parser.HaskellFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.charset.Charset;
import java.util.LinkedHashMap;
import java.util.Map;

// todo: copy from Groovy
public final class HaskellRunConfiguration extends RuntimeConfiguration implements CommonProgramRunConfigurationParameters {

    private static final String NO_GHC = "GHC location not specified";

    public HaskellFile mainFile;
    public String rtFlags;
    public String programParameters;
    public String workingDir;
    public boolean passParentEnvs = true;

    private Map<String, String> myEnvs = new LinkedHashMap<String, String>();

    public HaskellRunConfiguration(Project project, ConfigurationFactory factory) {
        super("Haskell", project, factory);
    }

    public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
        return new ConfigurationEditor(getProject());
    }

    public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
        HaskellState state = new HaskellState(env);
        state.setConsoleBuilder(TextConsoleBuilderFactory.getInstance().createBuilder(getProject()));
        return state;
    }

    public void setProgramParameters(@Nullable String value) {
        programParameters = value;
    }

    public String getProgramParameters() {
        return programParameters;
    }

    public void setWorkingDirectory(@Nullable String value) {
        workingDir = ExternalizablePath.urlValue(value);
    }

    public String getWorkingDirectory() {
        return ExternalizablePath.localPathValue(workingDir);
    }

    public void setEnvs(@NotNull Map<String, String> envs) {
        this.myEnvs = envs;
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

    @Override
    public String suggestedName() {
        return mainFile == null ? "Unnamed" : mainFile.getName();
    }

    // todo: read/write external???

    private final class HaskellState extends CommandLineState {

        HaskellState(ExecutionEnvironment environment) {
            super(environment);
        }

        protected ProcessHandler startProcess() throws ExecutionException {
            GeneralCommandLine commandLine;
            try {
                commandLine = ApplicationManager.getApplication().runReadAction(new Computable<GeneralCommandLine>() {
                    public GeneralCommandLine compute() {
                        try {
                            Sdk ghc = ProjectRootManager.getInstance(getProject()).getProjectSdk(); // todo: get module SDK
                            if (ghc == null) {
                                throw new CantRunException(NO_GHC);
                            }

                            SdkType sdkType = ghc.getSdkType();
                            if (!(sdkType instanceof HaskellSdkType)) {
                                throw new CantRunException(NO_GHC);
                            }

                            String exePath = ghc.getHomePath() + "/bin/runhaskell"; // todo
                            if (exePath == null) {
                                throw new CantRunException("Cannot find runhaskell executable");
                            }
                            if (mainFile == null) {
                                throw new CantRunException("Main module is not specified");
                            }

                            GeneralCommandLine commandLine = new GeneralCommandLine();
                            commandLine.setExePath(exePath);
                            Charset charset = Charset.forName("UTF-8");
                            commandLine.setCharset(charset);

                            Map<String, String> env = getEnvs();
                            commandLine.setEnvParams(env);
                            commandLine.setPassParentEnvs(isPassParentEnvs());

                            //commandLine.setWorkDirectory(getWorkingDirectory());
                            commandLine.setWorkDirectory("D:/TEMP/untitled"); // todo!!!

                            VirtualFile file = mainFile.getVirtualFile();
                            commandLine.addParameter(file.getPath()); // todo

                            // todo: set other parameters/rt flags

                            return commandLine;
                        } catch (CantRunException e) {
                            throw new RuntimeException(e);
                        }
                    }
                });
            } catch (RuntimeException ex) {
                if (ex.getCause() instanceof CantRunException) {
                    throw (CantRunException) ex.getCause();
                } else {
                    throw ex;
                }
            }
            return JavaCommandLineStateUtil.startProcess(commandLine);
        }
    }
}
