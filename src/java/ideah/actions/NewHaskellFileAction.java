package ideah.actions;

import com.intellij.CommonBundle;
import com.intellij.ide.IdeView;
import com.intellij.ide.actions.CreateElementActionBase;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.util.IncorrectOperationException;
import ideah.HaskellFileType;
import ideah.module.HaskellModuleType;
import org.jetbrains.annotations.NotNull;

public final class NewHaskellFileAction extends CreateElementActionBase {

    private static final String WHAT = "Haskell module";

    public NewHaskellFileAction() {
        super(WHAT, "Creates new " + WHAT, HaskellFileType.HASKELL_ICON); // todo: another icon?
    }

    @NotNull
    protected PsiElement[] invokeDialog(Project project, PsiDirectory directory) {
        if (!isHaskellModule(project, directory)) {
            Messages.showErrorDialog(project, "Cannot create " + WHAT + " in non-Haskell project", "Wrong module");
            return new PsiElement[0];
        }
        MyInputValidator validator = new MyInputValidator(project, directory);
        Messages.showInputDialog(project, "Enter name for new " + WHAT, "New " + WHAT, Messages.getQuestionIcon(), "", validator);
        return validator.getCreatedElements();
    }

    protected void checkBeforeCreate(String newName, PsiDirectory directory) throws IncorrectOperationException {
    }

    @NotNull
    protected PsiElement[] create(String newName, PsiDirectory directory) throws Exception {
        if ("".equals(newName)) {
            throw new IncorrectOperationException("A name should be specified.");
        }
        HaskellFileType type = HaskellFileType.INSTANCE;
        String ext = type.getDefaultExtension();
        Project project = directory.getProject();
        PsiDirectory moduleDir = directory;
        int length = newName.length() - 1;
        while (newName.charAt(length) == '.') {
            newName = newName.substring(0, length);
            length--;
        }
        String[] fileNames = newName.split("\\.");
        int depth = fileNames.length;
        String moduleName = fileNames[depth - 1];
        boolean needsModuleName = Character.isUpperCase(moduleName.charAt(0));
        String parentPackages = ProjectRootManager.getInstance(project).getFileIndex().getPackageNameByDirectory(directory.getVirtualFile());
        StringBuilder packages = new StringBuilder(
                "".equals(parentPackages) || !needsModuleName
                        ? ""
                        : parentPackages + "."
        );
        for (int i = 0; i < depth - 1; i++) {
            String dirName = fileNames[i];
            if ("".equals(dirName)) {
                throw new IncorrectOperationException("File name cannot be empty.");
            }
            PsiDirectory subDir = moduleDir.findSubdirectory(dirName);
            moduleDir = subDir == null ? moduleDir.createSubdirectory(dirName) : subDir;
            if (needsModuleName) {
                packages.append(dirName).append('.');
            }
        }
        PsiFile file = PsiFileFactory.getInstance(project).createFileFromText(moduleName + "." + ext, type,
                needsModuleName
                        ? "module " + packages + moduleName + " where\n\n"
                        : ""
        );
        file = (PsiFile) moduleDir.add(file);
        VirtualFile virtualFile = file.getVirtualFile();
        if (virtualFile != null) {
            FileEditorManager.getInstance(directory.getProject()).openFile(virtualFile, true);
        }
        return new PsiElement[] {file};
    }

    protected String getErrorTitle() {
        return CommonBundle.getErrorTitle();
    }

    protected String getCommandName() {
        return "Create " + WHAT;
    }

    protected String getActionName(PsiDirectory directory, String newName) {
        return WHAT;
    }

    @Override
    protected boolean isAvailable(DataContext dataContext) {
        if (!super.isAvailable(dataContext))
            return false;

        Project project = PlatformDataKeys.PROJECT.getData(dataContext);
        if (project == null) {
            return false;
        }

        IdeView view = LangDataKeys.IDE_VIEW.getData(dataContext);
        if (view == null) {
            return false;
        }

        PsiDirectory[] dirs = view.getDirectories();
        if (dirs == null || dirs.length <= 0)
            return false;
        boolean anyHaskell = false;
        for (PsiDirectory dir : dirs) {
            if (isHaskellModule(project, dir)) {
                anyHaskell = true;
                break;
            }
        }

        return anyHaskell;
    }

    private static boolean isHaskellModule(Project project, PsiDirectory dir) {
        Module module = ProjectRootManager.getInstance(project).getFileIndex().getModuleForFile(dir.getVirtualFile());
        if (module == null)
            return false;
        return HaskellModuleType.INSTANCE.equals(module.getModuleType());
    }
}
