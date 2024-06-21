package com.kanven.compiler.analyzer;

import com.kanven.compiler.analyzer.domain.Node;
import com.kanven.compiler.lexical.Lexer;
import com.kanven.compiler.lexical.TokenStream;
import com.kanven.compiler.lexical.Tokenizer;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.domain.TokenType;

import java.util.List;

public class Compare extends Analyzer {

    private Expression expression;

    public Compare(TokenStream stream) {
        super(stream);
        this.expression = new Expression(stream);
        this.callback = () -> parseCompare();
    }

    @Override
    public void setCallback(Callback callback) {
        this.expression.callback = callback;
    }

    @Override
    public Node<Token> parse() {
        this.ast = parseCompare();
        return this.ast;
    }

    protected Node<Token> parseCompare() {
        Node<Token> left = expression.parse();
        Node<Token> right = parseNewCompare();
        if (right != null) {
            right.left(left);
            return right;
        }
        return left;
    }

    private Node<Token> parseNewCompare() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            Token token = stream.current();
            if (TokenType.isCmp(token)) {
                node = new Node<>(token);
                stream.move();
                node.right(parseCompare());
            }
        }
        return node;
    }

    public static void main(String[] args) {
        String exp = "(a + b) * c > d + e";
        Tokenizer tokenizer = new Tokenizer(exp);
        Lexer lexical = new Lexer();
        List<Token> tokens = lexical.parse(tokenizer);
        Lexer.printTokens(tokens);
        TokenStream stream = new TokenStream(tokens);
        Compare parser = new Compare(stream);
        parser.parse();
    }

}
