package haskell.parser;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElementVisitor;
import haskell.HaskellFileType;
import org.jetbrains.annotations.NotNull;

public final class HaskellFileImpl extends PsiFileBase implements HaskellFile {

    public HaskellFileImpl(@NotNull FileViewProvider provider) {
        super(provider, HaskellFileType.HASKELL_LANGUAGE);
    }

    @NotNull
    public FileType getFileType() {
        return HaskellFileType.INSTANCE;
    }

    public void accept(@NotNull PsiElementVisitor visitor) {
        visitor.visitFile(this); // todo
    }

    public boolean isMainModule() {
        String name = getName();
        String baseName = FileUtil.getNameWithoutExtension(name);
        return "Main".equals(baseName) || (baseName.length() > 0 && Character.isLowerCase(baseName.charAt(0)));
    }
}
