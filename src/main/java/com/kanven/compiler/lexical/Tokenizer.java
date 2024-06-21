package com.kanven.compiler.lexical;

public class Tokenizer {

    private final String expr;

    private final char[] chs;

    private int pos = 0;

    private int lower = 0;

    public Tokenizer(String expr) {
        this.expr = expr;
        this.chs = expr.toCharArray();
    }

    public boolean hasNext() {
        return pos < chs.length;
    }


    public Character nextChar() {
        for (int cursor = this.pos + 1; cursor < this.chs.length; cursor++) {
            char ch = chs[cursor];
            if (!Character.isWhitespace(ch)) {
                return ch;
            }
        }
        return null;
    }

    public String expr() {
        return this.expr;
    }

    public char pre() {
        return chs[pos - 1];
    }

    public char current() {
        return chs[pos];
    }

    public Character next() {
        if (pos + 1 > chs.length) {
            return null;
        }
        return chs[pos + 1];
    }

    public int pos() {
        return this.pos;
    }

    public String str(int upper) {
        StringBuilder builder = new StringBuilder();
        for (int i = this.lower; i <= upper; i++) {
            builder.append(Character.toString(chs[i]));
        }
        return builder.toString();
    }

    public void lower(int pos) {
        this.lower = pos;
    }

    public int lower() {
        return this.lower;
    }

    public void move(int step) {
        this.pos += 2;
    }

    public void move() {
        this.pos++;
    }

    public void skipWhitespace() {
        while (hasNext()) {
            char ch = current();
            if (Character.isWhitespace(ch)) {
                move();
                lower(pos());
            } else {
                break;
            }
        }
    }

}
