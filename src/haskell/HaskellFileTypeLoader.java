package haskell;

import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public final class HaskellFileTypeLoader extends FileTypeFactory {

    public static final List<FileType> HASKELL_FILE_TYPES = new ArrayList<FileType>();

    public void createFileTypes(@NotNull FileTypeConsumer consumer) {
        consumer.consume(HaskellFileType.INSTANCE, StringUtil.join(new String[] {"hs", "lhs"}, ";"));
        HASKELL_FILE_TYPES.add(HaskellFileType.INSTANCE);
    }
}
