package com.kanven.compiler.lexical.domain;

public final class Token {

    private TokenType type;

    private final String symbol;

    private int start = 0;

    private int end = 0;

    public Token(String symbol, int start, int end) {
        this.symbol = symbol;
        this.start = start;
        this.end = end;
    }

    public Token(TokenType type, String symbol) {
        this.type = type;
        this.symbol = symbol;
    }

    public Token(TokenType type, String symbol, int start, int end) {
        this(type, symbol);
        this.start = start;
        this.end = end;
    }

    public String symbol() {
        return this.symbol;
    }

    public TokenType type() {
        return this.type;
    }

    public int stat() {
        return start;
    }

    public int end() {
        return end;
    }

    @Override
    public String toString() {
        return "{\"type\":\"" + type + "\", \"symbol\":\"" + symbol + "\",\"start\":\"" + start + "\",\"end\":\"" + end + "\"}";
    }
}
