package ideah.compiler;

import com.intellij.openapi.compiler.CompilerMessageCategory;
import ideah.util.LineColRange;

public final class GHCMessage {

    private final LineColRange range;
    private final String errorMessage;
    private final CompilerMessageCategory category;
    private final String fileName;

    GHCMessage(String ghcError) {
        String newLine = LaunchGHC.EOLN;
        int newLineIndex = ghcError.indexOf(newLine);
        fileName = ghcError.substring(0, newLineIndex);

        int nextNewLineIndex = ghcError.indexOf(newLine, newLineIndex + newLine.length());
        String posString = ghcError.substring(newLineIndex + newLine.length(), nextNewLineIndex);
        CompilerMessageCategory cmc;
        if (posString.startsWith("W")) {
            cmc = CompilerMessageCategory.WARNING;
            posString = posString.substring(1);
        } else if (posString.startsWith("E")) {
            cmc = CompilerMessageCategory.ERROR;
            posString = posString.substring(1);
        } else {
            cmc = CompilerMessageCategory.ERROR;
        }
        range = new LineColRange(posString);

        errorMessage = ghcError.substring(nextNewLineIndex + newLine.length());
        category = cmc;
    }

    GHCMessage(String errorMessage, String fileName) {
        this.range = new LineColRange(1, 1, 1, 1);
        this.errorMessage = errorMessage;
        this.category = CompilerMessageCategory.ERROR;
        this.fileName = fileName;
    }

    public LineColRange getRange() {
        return range;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public CompilerMessageCategory getCategory() {
        return category;
    }

    public String getFileName() {
        return fileName;
    }
}
