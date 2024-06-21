package com.kanven.compiler.analyzer;


import com.kanven.compiler.SyntaxException;
import com.kanven.compiler.analyzer.domain.Node;
import com.kanven.compiler.lexical.Lexer;
import com.kanven.compiler.lexical.TokenStream;
import com.kanven.compiler.lexical.Tokenizer;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.domain.TokenType;
import com.kanven.compiler.lexical.grammar.Grammar;
import com.kanven.compiler.lexical.grammar.LLPreprocessor;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class LLAnalyzer {

    protected Node<Token> ast;

    private LLPreprocessor processor;

    private Grammar grammar;

    public LLAnalyzer(Grammar grammar) {
        this.grammar = grammar;
        this.processor = new LLPreprocessor(grammar);
    }

    public void parse(TokenStream stream) {
        Stack<Sub> subs = new Stack<>();
        String non = grammar.start();
        while (stream.hasToken() || !subs.isEmpty()) {
            Sub sub = doNext(non, stream);
            if (sub == null) {
                sub = subs.pop();
            }
            Result result = sub.match();
            if (sub.isEnd()) {
                buildTree(subs, sub);
            } else {
                subs.push(sub);
            }
            non = result.expression;
        }
    }

    private void buildTree(Stack<Sub> subs, Sub sub) {
        Node<Token> node = sub.node();
        if (subs.isEmpty()) {
            //解析子树完成
            if (this.ast == null) {
                this.ast = node;
            } else {
                if (node.left() == null) {
                    node.left(this.ast);
                    this.ast = node;
                } else if (node.right() == null) {
                    this.ast.right(node);
                } else {
                    throw new SyntaxException();
                }
            }
        } else {
            Sub top = subs.peek();
            top.node(node);
        }
    }


    private Sub doNext(String non, TokenStream stream) {
        Map<String, List<String>> expressions = processor.flow().get(non);
        if (stream.hasToken()) {
            Token token = stream.current();
            if (expressions != null) {
                List<String> expression;
                if (token.type() == TokenType.INF) {
                    expression = expressions.get("INF");
                } else {
                    expression = expressions.get(token.symbol());
                }
                if (expression == null) {
                    return null;
                }
                return new Sub(non, expression, stream);
            }
        }
        return null;
    }

    private class Sub {

        private String parent;

        private TokenStream stream;

        private List<String> expression;

        private int cursor = 0;

        private List<Node<Token>> nodes = new ArrayList<>();

        private Node<Token> node;

        public Sub(String parent, List<String> expression, TokenStream stream) {
            this.parent = parent;
            this.expression = expression;
            this.stream = stream;
        }

        public boolean isEnd() {
            return cursor == this.expression.size();
        }

        public Result match() {
            if (cursor >= expression.size()) {
                return new Result(true, expression.get(cursor - 1));
            }
            boolean flag = false;
            if (stream.hasToken()) {//保证token stream 结束后，匹配过程走完
                Token token = stream.current();
                String non = expression.get(cursor);
                if (TokenType.INF.name().equals(non)) {
                    stream.move();
                    this.nodes.add(new Node<>(token));
                    flag = true;
                } else if (token.symbol().equals(non)) {
                    stream.move();
                    if (grammar.isTerminator(token.symbol()) || grammar.isIdentifier(token.symbol())) {
                        this.nodes.add(new Node<>(token));
                    }
                    flag = true;
                }
            }
            Result result = new Result(flag, expression.get(cursor++));
            if (isEnd()) {
                this.node = buildSub();
            }
            return result;
        }

        private Node<Token> node() {
            return this.node;
        }


        private Node<Token> buildSub() {
            if (nodes.isEmpty()) {
                return null;
            }
            Node<Token> first = nodes.get(0);
            int len = nodes.size();
            if (len == 1) {
                return first;
            }
            int i = len - 1;
            Node<Token> tail = nodes.get(i--);
            Node<Token> cursor = tail;
            while (i > 0) {
                //处理大于2个表达式
                Node<Token> pre = nodes.get(i--);
                if (cursor.left() == null) {
                    cursor.left(pre);
                    cursor = pre;
                } else if (pre.right() == null) {
                    pre.right(cursor);
                    cursor = pre;
                } else {
                    throw new SyntaxException();
                }
            }
            if (TokenType.isInf(first.value())) {
                //需要兼容fist 为INF的情况
                tail.left(first);
                return tail;
            } else {
                if (first.right() == null) {
                    first.right(tail);
                    return first;
                } else {
                    //优先级在左侧，左子树是满树
                    if (tail.left() == null) {
                        tail.left(first);
                        return tail;
                    }
                    throw new SyntaxException();
                }
            }
        }

        public void node(Node<Token> node) {
            if (node != null) {
                this.nodes.add(node);
            }
        }

        @Override
        public String toString() {
            return "{\"parent\":" + parent + ",\"expression\":" + (expression == null ? "" : expression) + ",\"cursor\":" + cursor + ", \"nodes\":" + nodes + "}";
        }

    }

    private class Result {

        private boolean matched = false;

        private String expression;

        public Result(boolean matched, String expression) {
            this.matched = matched;
            this.expression = expression;
        }

        @Override
        public String toString() {
            return "{\"matched\":" + matched + ", \"expression\":" + expression + "}";
        }
    }


    public static void main(String[] args) throws IOException {
        String exp = "(a + e) * t > f + g and not (b or c) ";
        //String exp = "not a";
        //String exp = "(a + e) * t > f + g";
        //String exp = "((a + b)/d) * (c + e/f)";
        //String exp = "a*(b+c)";
        //String exp = "(a+b)*c";
        //String exp = "a + b*c*d/e";
        //String exp = "a + b*c";
        //String exp = "a + b";
        Tokenizer tokenizer = new Tokenizer(exp);
        Lexer lexical = new Lexer();
        List<Token> tokens = lexical.parse(tokenizer);
        Lexer.printTokens(tokens);
        TokenStream stream = new TokenStream(tokens);
        Grammar grammar = new Grammar("grammar.txt");
        LLAnalyzer ll = new LLAnalyzer(grammar);
        ll.parse(stream);
        System.out.println("====end====");
    }

}
