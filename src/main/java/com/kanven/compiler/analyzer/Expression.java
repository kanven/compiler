package com.kanven.compiler.analyzer;


import com.kanven.compiler.analyzer.domain.Node;
import com.kanven.compiler.lexical.Lexer;
import com.kanven.compiler.lexical.TokenStream;
import com.kanven.compiler.lexical.Tokenizer;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.symbol.Symbol;

import java.util.List;


public class Expression extends Analyzer {


    public Expression(TokenStream stream) {
        super(stream);
        callback = () -> parseExpress();
    }

    public Node<Token> parse() {
        this.ast = parseExpress();
        return this.ast;
    }

    /**
     * 表达式解析
     */
    protected Node<Token> parseExpress() {
        Node<Token> left = parseTerm();
        Node<Token> right = parseNewExpress();
        if (right != null) {
            right.left(left);
        }
        return right == null ? left : right;
    }

    private Node<Token> parseTerm() {
        Node<Token> left = parseFactor();
        Node<Token> right = parseNewTerm();
        if (right != null) {
            right.left(left);
        }
        return right == null ? left : right;
    }

    private Node<Token> parseNewExpress() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            Token token = stream.current();
            //采用FIRST集合匹配正确语句，消除回溯
            if (token.symbol().equals(Character.toString(Symbol.ADD)) || token.symbol().equals(Character.toString(Symbol.SUB))) {
                //匹配成功
                node = new Node<>(token);
                //关注点前移一位
                stream.move();
                Node<Token> expr = parseExpress();
                node.right(expr);
            }
        }
        return node;
    }


    private Node<Token> parseNewTerm() {
        Node<Token> node = null;
        if (stream.hasToken()) {
            Token token = stream.current();
            if (token.symbol().equals(Character.toString(Symbol.MULT)) || token.symbol().equals(Character.toString(Symbol.DIVIDE))) {
                //匹配成功
                node = new Node<>(token);
                //关注点前移一位
                stream.move();
                Node<Token> right = parseTerm();
                node.right(right);
            }
        }
        return node;
    }

    public static void main(String[] args) {
        String exp = "((a + b)/d) * c";
        Tokenizer tokenizer = new Tokenizer(exp);
        Lexer lexical = new Lexer();
        List<Token> tokens = lexical.parse(tokenizer);
        Lexer.printTokens(tokens);
        TokenStream stream = new TokenStream(tokens);
        Expression parser = new Expression(stream);
        parser.parse();
    }

}
