package haskell.compiler;

import com.intellij.openapi.compiler.CompilerMessageCategory;

public final class GHCMessage {

    private final int startLine;
    private final int endLine;
    private final int startColumn;
    private final int endColumn;
    private final String errorMessage;
    private final CompilerMessageCategory category;
    private final String fileName;

    private static int parseInt(String str) {
        str = str.trim();
        if ("?".equals(str))
            return 1;
        return Integer.parseInt(str);
    }

    private static int column(int value) {
        return value <= 0 ? 1 : value;
    }

    GHCMessage(String ghcError) {
        String newLine = GHCMessageReader.EOLN;
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
        String[] lineColumnStrings = posString.split("[-:]");
        startLine = parseInt(lineColumnStrings[0]);
        startColumn = column(parseInt(lineColumnStrings[1]));
        endLine = parseInt(lineColumnStrings[2]);
        endColumn = column(parseInt(lineColumnStrings[3]));

        errorMessage = ghcError.substring(nextNewLineIndex + newLine.length());
        category = cmc;
    }

    GHCMessage(String errorMessage, String fileName) {
        this.startLine = 1;
        this.endLine = 1;
        this.startColumn = 1;
        this.endColumn = 1;
        this.errorMessage = errorMessage;
        this.category = CompilerMessageCategory.ERROR;
        this.fileName = fileName;
    }

    public int getStartLine() {
        return startLine;
    }

    public int getEndLine() {
        return endLine;
    }

    public int getStartColumn() {
        return startColumn;
    }

    public int getEndColumn() {
        return endColumn;
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
