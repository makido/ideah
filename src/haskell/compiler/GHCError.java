package haskell.compiler;

// todo: make unpublic final
// todo: make all methods unpublic
public class GHCError {

    // todo: make all final
    private int startLine, endLine;
    private int startColumn, endColumn;
    private String errorMessage;
    // todo: add warning flag

    public GHCError(String ghcError) {
        final String newLine = System.getProperty("line.separator"); // todo: remove final
        int newLineIndex = ghcError.indexOf(newLine);
        initPos(ghcError.substring(0, newLineIndex));
        errorMessage = ghcError.substring(newLineIndex).trim();
    }

    private void initPos(String posString) {
        String[] lineColumnStrings = posString.split("[-:]");
        // todo: parseInt
        startLine = Integer.valueOf(lineColumnStrings[0]);
        startColumn = Integer.valueOf(lineColumnStrings[1]);
        endLine = Integer.valueOf(lineColumnStrings[2]);
        endColumn = Integer.valueOf(lineColumnStrings[3]);
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
}
