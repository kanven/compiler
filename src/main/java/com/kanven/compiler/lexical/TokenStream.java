package com.kanven.compiler.lexical;

import com.kanven.compiler.lexical.domain.Token;

import java.util.List;

public class TokenStream {

    private final List<Token> tokens;

    private int pos = 0;

    private final int len;

    public TokenStream(List<Token> tokens) {
        this.tokens = tokens;
        this.len = tokens == null ? 0 : tokens.size();
    }

    public boolean hasToken() {
        return pos + 1 <= len;
    }

    public Token current() {
        return tokens.get(pos);
    }

    public void move() {
        ++pos;
    }

    /**
     * 预取下一个Token
     *
     * @return
     */
    public Token prefetch() {
        if (pos + 1 < len) {
            return tokens.get(pos + 1);
        }
        return null;
    }

}
