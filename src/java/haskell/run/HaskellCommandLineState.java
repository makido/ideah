package haskell.run;

import com.intellij.execution.CantRunException;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.JavaCommandLineStateUtil;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.util.ProgramParametersUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Computable;
import haskell.compiler.HaskellSdkType;
import haskell.util.CompilerLocation;

import java.nio.charset.Charset;
import java.util.Map;

final class HaskellCommandLineState extends CommandLineState {

    private static final String NO_GHC = "GHC valid location not specified";

    private HaskellParameters myParams;

    private final HaskellRunConfiguration configuration;

    HaskellCommandLineState(ExecutionEnvironment environment, HaskellRunConfiguration configuration) {
        super(environment);
        this.configuration = configuration;
    }

    public HaskellParameters getHaskellParameters() {
        if (myParams == null) {
            myParams = createHaskellParameters();
        }
        return myParams;
    }

    private HaskellParameters createHaskellParameters() {
        HaskellParameters params = new HaskellParameters();
        Module module = configuration.getModule();
        ProgramParametersUtil.configureConfiguration(params, configuration);
        params.configureByModule(module);
        params.setMainFile(configuration.getMainFile());
        params.setRuntimeFlags(configuration.getRuntimeFlags());
        return params;
    }

    protected ProcessHandler startProcess() throws ExecutionException {
        return JavaCommandLineStateUtil.startProcess(createCommandLine());
    }

    private GeneralCommandLine createCommandLine() throws ExecutionException {
        try {
            return ApplicationManager.getApplication().runReadAction(new Computable<GeneralCommandLine>() {
                public GeneralCommandLine compute() {
                    try {
                        HaskellParameters parameters = getHaskellParameters();
                        Sdk ghc = parameters.getGhc();
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
                        String mainFile = parameters.getMainFile();
                        if (mainFile == null) {
                            throw new CantRunException("Main module is not specified");
                        }

                        GeneralCommandLine commandLine = new GeneralCommandLine();
                        commandLine.setExePath(exePath);
                        Charset charset = Charset.forName("UTF-8");
                        commandLine.setCharset(charset);

                        Map<String, String> env = parameters.getEnv();
                        if (env != null) {
                            commandLine.setEnvParams(env);
                            commandLine.setPassParentEnvs(parameters.isPassParentEnvs());
                        }

                        commandLine.setWorkDirectory(parameters.getWorkingDirectory());

                        commandLine.addParameter("-i" + CompilerLocation.rootsToString(
                                ModuleRootManager.getInstance(configuration.getModule()).getSourceRoots(false)));
                        commandLine.addParameter(mainFile); // todo

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
    }
}
