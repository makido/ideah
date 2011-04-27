package haskell.compiler;

final class GHCError {

    // todo: make all final
    private int startLine, endLine;
    private int startColumn, endColumn;
    private String errorMessage;
    // todo: add warning flag

    GHCError(String ghcError) {
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

    int getStartLine() {
        return startLine;
    }

    int getEndLine() {
        return endLine;
    }

    int getStartColumn() {
        return startColumn;
    }

    int getEndColumn() {
        return endColumn;
    }

    String getErrorMessage() {
        return errorMessage;
    }
}
