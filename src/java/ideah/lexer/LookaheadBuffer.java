package ideah.lexer;

/**
 * ����� ��� ��������� �������� "������" �� ������.
 */
final class LookaheadBuffer {

    /**
     * ����������� ����� - �������� � ���� ������� ��� -1 ��� ����� �����
     */
    private final int[] buf;
    /**
     * ������� ������� � ����������� ������
     */
    private int pos = 0;

    /**
     * �������� ������ - ����� ��������
     */
    private CharSequence source;
    private int posIndex = 0;
    /**
     * ������� ������� � ����� (�� 0) - ������ ������� peek(0)
     */
    private int index = 0;
    private int endIndex = 0;

    /**
     * @param length ����� ������
     */
    LookaheadBuffer(int length) {
        buf = new int[length];
    }

    public void init(CharSequence str, int startIndex, int endIndex) {
        this.source = str;
        this.index = this.posIndex = startIndex;
        this.endIndex = endIndex;
        for (int i = 0; i < buf.length; i++) {
            buf[i] = read(posIndex++);
        }
        pos = 0;
    }

    /**
     * ������ ������ ������� �� ������
     */
    private int read(int index) {
        if (index >= endIndex) {
            return -1;
        } else {
            return source.charAt(index);
        }
    }

    /**
     * �� ��, ��� peek(0)
     *
     * @see #peek(int)
     */
    public int peek() {
        return peek(0);
    }

    /**
     * ��������� ���������� ������� ��������� ������.
     *
     * @param la �������� � ������ (lookahead)
     * @return ������ �� ������� �������� ��� -1, ���� �������� ��������� �� ��������� �����
     */
    public int peek(int la) {
        assert la < buf.length;
        return buf[(pos + la) % buf.length];
    }

    /**
     * ������ ���������� ������� � ����� (������ ������ - peek(0) - ��� ���� ��������).
     *
     * @return �������� ������� �������, ������� ���� ��������� �� ������
     */
    public void next() {
        buf[pos] = read(posIndex++);
        pos = (pos + 1) % buf.length;
        if (index < endIndex) {
            index++;
        }
    }

    /**
     * ����� ��� �������� (convenience method). ��������� ������� ������� � ������; ����
     * ���� ������ ����, �� �� "��������������".
     *
     * @param ch ������
     * @return true ���� ����� ������ � ������ ��� (��� ���� ��� ������ ��� ���������
     *         ������� {@link #next()}; false, ���� ������ ������� �� ���� (��� ���� ���������
     *         � ������ ���)
     */
    public boolean match(int ch) {
        if (peek() == ch) {
            next();
            return true;
        } else {
            return false;
        }
    }

    /**
     * ���������, ��������� �� ����� �����.
     *
     * @return true, ���� {@link #peek()} ���������� -1
     */
    public boolean eof() {
        return peek() < 0;
    }

    /**
     * ���������� ���������� �������, ���������������� {@link #peek()}.
     *
     * @return ���������� ������� ������� � ������
     */
    public int getCoords() {
        return index;
    }
}
