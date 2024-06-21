package com.kanven.compiler.analyzer;


import com.kanven.compiler.analyzer.domain.Node;
import com.kanven.compiler.lexical.Lexer;
import com.kanven.compiler.lexical.TokenStream;
import com.kanven.compiler.lexical.Tokenizer;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.domain.TokenType;

import java.util.List;

public class Logic extends Analyzer {

    private Analyzer compare;

    public Logic(TokenStream stream) {
        super(stream);
        this.compare = new Compare(stream);
        this.compare.setCallback(() -> parseStatement());
    }

    public Node<Token> parse() {
        this.ast = parseStatement();
        return this.ast;
    }

    protected Node<Token> parseStatement() {
        Node<Token> left = parseAnd();
        Node<Token> right = parseNewStatement();
        if (right != null) {
            right.left(left);
            return right;
        }
        return left;
    }

    private Node<Token> parseNewStatement() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            //获取当前关注点
            Token token = stream.current();
            if (TokenType.OR.matchType(token)) {
                //OR运算符处理
                node = new Node<>(token);
                stream.move();
                node.right(parseStatement());
            }
        }
        return node;
    }

    private Node<Token> parseAnd() {
        Node<Token> left = parseNot();
        Node<Token> right = parseNewAnd();
        if (right != null) {
            right.left(left);
            return right;
        }
        return left;
    }

    private Node<Token> parseNewAnd() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            Token token = stream.current();
            if (TokenType.AND.matchType(token)) {
                node = new Node<>(token);
                stream.move();
                node.right(parseAnd());
            }
        }
        return node;
    }

    private Node<Token> parseNot() {
        Node<Token> left = compare.parse();
        Node<Token> right = parseNewNot();
        if (right != null) {
            right.left(left);
            return right;
        }
        return left;
    }

    private Node<Token> parseNewNot() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            Token token = stream.current();
            if (TokenType.NOT.matchType(token)) {
                node = new Node<>(token);
                stream.move();
                node.right(parseNot());
            }
        }
        return node;
    }

    public static void main(String[] args) {
        String exp = "(a + e) * t > f + g and not (b or c) ";
        Tokenizer tokenizer = new Tokenizer(exp);
        Lexer lexer = new Lexer();
        List<Token> tokens = lexer.parse(tokenizer);
        Lexer.printTokens(tokens);
        TokenStream stream = new TokenStream(tokens);
        Logic logic = new Logic(stream);
        logic.parse();
    }

}
