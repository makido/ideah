package haskell.module;

import com.intellij.ide.util.projectWizard.JavaModuleBuilder;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.ProjectRootManager;
import haskell.compiler.HaskellSdkType;

// todo: setup page?
public final class HaskellModuleBuilder extends JavaModuleBuilder {

    @Override
    public HaskellModuleType getModuleType() {
        return HaskellModuleType.INSTANCE;
    }

    @Override
    public boolean isSuitableSdk(Sdk sdk) {
        return sdk.getSdkType() == HaskellSdkType.INSTANCE;
    }

    @Override
    public void setupRootModel(ModifiableRootModel rootModel) throws ConfigurationException {
        ProjectJdkTable table = ProjectJdkTable.getInstance();
        Sdk[] sdks = table.getAllJdks();
        Sdk ghc = null;
        for (Sdk sdk : sdks) {
            if (sdk.getSdkType().equals(HaskellSdkType.INSTANCE)) {
                ghc = sdk;
                break;
            }
        }
        if (ghc == null) {
            ghc = SdkConfigurationUtil.findOrCreateSdk(HaskellSdkType.INSTANCE);
        }
        if (ghc != null) {
            Project project = rootModel.getProject();
            // todo: do not reset if overriden by user?
            ProjectRootManager.getInstance(project).setProjectSdk(ghc);
            setModuleJdk(ghc); // todo: inherit SDK from project?
        }
        // todo: do not use tabs in project
        super.setupRootModel(rootModel);
    }
}
