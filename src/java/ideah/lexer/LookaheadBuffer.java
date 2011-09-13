package ideah.lexer;

/**
 * Буфер для просмотра символов "вперед" по потоку.
 */
final class LookaheadBuffer {

    /**
     * Циклический буфер - содержит в себе символы или -1 для конца файла
     */
    private final int[] buf;
    /**
     * Текущая позиция в циклическом буфере
     */
    private int pos = 0;

    /**
     * Источник данных - поток символов
     */
    private CharSequence source;
    private int posIndex = 0;
    /**
     * Текущая позиция в файле (от 0) - индекс символа peek(0)
     */
    private int index = 0;
    private int endIndex = 0;

    /**
     * @param length длина буфера
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
     * Чтение одного символа из потока
     */
    private int read(int index) {
        if (index >= endIndex) {
            return -1;
        } else {
            return source.charAt(index);
        }
    }

    /**
     * То же, что peek(0)
     *
     * @see #peek(int)
     */
    public int peek() {
        return peek(0);
    }

    /**
     * Позволяет посмотреть текущее состояние буфера.
     *
     * @param la смещение в буфере (lookahead)
     * @return символ по данному смещению или -1, если смещение находится за пределами файла
     */
    public int peek(int la) {
        assert la < buf.length;
        return buf[(pos + la) % buf.length];
    }

    /**
     * Чтение следующего символа в буфер (первый символ - peek(0) - при этом теряется).
     *
     * @return значение первого символа, которое было вытеснено из буфера
     */
    public void next() {
        buf[pos] = read(posIndex++);
        pos = (pos + 1) % buf.length;
        if (index < endIndex) {
            index++;
        }
    }

    /**
     * Метод для удобства (convenience method). Проверяет наличие символа в потоке; если
     * этот символ есть, то он "проглатывается".
     *
     * @param ch символ
     * @return true если такой символ в потоке был (при этом сам символ был проглочен
     *         вызовом {@link #next()}; false, если такого символа не было (при этом изменений
     *         в буфере нет)
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
     * Проверяет, достигнут ли конец файла.
     *
     * @return true, если {@link #peek()} возвращает -1
     */
    public boolean eof() {
        return peek() < 0;
    }

    /**
     * Возвращает координаты символа, соответствующего {@link #peek()}.
     *
     * @return координаты первого символа в буфере
     */
    public int getCoords() {
        return index;
    }
}
