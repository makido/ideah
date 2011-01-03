package haskell.actions;

import com.intellij.CommonBundle;
import com.intellij.ide.actions.CreateElementActionBase;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.util.IncorrectOperationException;
import haskell.HaskellFileType;
import org.jetbrains.annotations.NotNull;

public final class NewHaskellFileAction extends CreateElementActionBase {

    private static final String WHAT = "Haskell module";

    public NewHaskellFileAction() {
        super(WHAT, "Creates new " + WHAT, HaskellFileType.HASKELL_ICON);
    }

    @NotNull
    protected PsiElement[] invokeDialog(Project project, PsiDirectory directory) {
        MyInputValidator validator = new MyInputValidator(project, directory);
        Messages.showInputDialog(project, "Enter name for new " + WHAT, "New " + WHAT, Messages.getQuestionIcon(), "", validator);
        return validator.getCreatedElements();
    }

    protected void checkBeforeCreate(String newName, PsiDirectory directory) throws IncorrectOperationException {
        // todo: check module name for validity
    }

    @NotNull
    protected PsiElement[] create(String newName, PsiDirectory directory) throws Exception {
        HaskellFileType type = HaskellFileType.INSTANCE;
        String ext = type.getDefaultExtension();
        Project project = directory.getProject();
        // todo: if module name is not valid, create empty file
        PsiFile file = PsiFileFactory.getInstance(project).createFileFromText(newName + "." + ext, type, "module " + newName + " where\n\n");
        // todo: smth not working!
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
        return super.isAvailable(dataContext); // todo: only for Haskell modules?
    }
}
