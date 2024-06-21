package com.kanven.compiler.analyzer;

import com.kanven.compiler.SyntaxException;
import com.kanven.compiler.analyzer.domain.Node;
import com.kanven.compiler.lexical.TokenStream;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.domain.TokenType;

public abstract class Analyzer {

    protected Node<Token> ast;

    protected TokenStream stream;

    protected Callback callback;

    public Analyzer(TokenStream stream) {
        this.stream = stream;
    }

    public abstract Node<Token> parse();

    public Node<Token> ast() {
        return ast;
    }

    protected Node<Token> parseFactor() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            Token token = stream.current();
            if (token.type() == TokenType.INF) {
                //匹配成功
                node = new Node<>(token);
                // 关注点前移一位
                stream.move();
            } else if (token.type() == TokenType.LEFT_BRACKET) {
                //匹配成功
                // 关注点前移一位
                stream.move();
                node = callback.parseEnter();
                //跳过右括号)
                stream.move();
            }
        }
        return node;
    }

    public void setCallback(Callback callback) {
        this.callback = callback;
    }


    public interface Callback {

        Node<Token> parseEnter();

    }


    protected void throwSyntaxException(Token token) {
        throw new SyntaxException("the expression has an syntax in:" + token);
    }

}
