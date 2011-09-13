package ideah.lexer;

/**
 * Описание лексемы
 */
public final class HaskellToken {

    /**
     * Тип лексемы
     */
    public final HaskellTokenType type;
    /**
     * Текст лексемы
     */
    public final String text;
    /**
     * Координаты первого символа
     */
    public final int coords;

    public HaskellToken(HaskellTokenType type, String text, int coords) {
        this.type = type;
        this.text = text;
        this.coords = coords;
    }

    public String toString() {
        return type + " " + text + " at " + coords;
    }
}
