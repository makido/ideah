package haskell.compiler;

public final class GHCMessage {

    private int startLine;
    private int endLine;
    private int startColumn;
    private int endColumn;
    private final String errorMessage;
    private final boolean isWarning;
    private final String fileName;

    public GHCMessage(String ghcError) {
        String newLine = System.getProperty("line.separator");
        int newLineIndex = ghcError.indexOf(newLine);
        fileName = ghcError.substring(0, newLineIndex).trim();
        int nextNewLineIndex = ghcError.indexOf(newLine, newLineIndex + 1);
        initPos(ghcError.substring(newLineIndex, nextNewLineIndex).trim());
        errorMessage = ghcError.substring(nextNewLineIndex).trim();
        isWarning = errorMessage.startsWith("Warning:");
    }

    private void initPos(String posString) {
        String[] lineColumnStrings = posString.split("[-:]");
        startLine = Integer.parseInt(lineColumnStrings[0]);
        startColumn = Integer.parseInt(lineColumnStrings[1]);
        endLine = Integer.parseInt(lineColumnStrings[2]);
        endColumn = Integer.parseInt(lineColumnStrings[3]);
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

    public boolean isWarning() {
        return isWarning;
    }

    public String getFileName() {
        return fileName;
    }
}
