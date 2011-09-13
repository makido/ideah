package ideah.lexer;

/**
 * �������� �������
 */
public final class HaskellToken {

    /**
     * ��� �������
     */
    public final HaskellTokenType type;
    /**
     * ����� �������
     */
    public final String text;
    /**
     * ���������� ������� �������
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
