package com.kanven.compiler.lexical;

import com.kanven.compiler.SyntaxException;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.domain.TokenType;
import com.kanven.compiler.lexical.symbol.Symbol;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class Lexer {

    private List<Token> tokens = new ArrayList<>(0);
    /**
     * 辅助子表达式分析
     */
    private Stack<Character> exps = new Stack<>();

    public Lexer() {

    }

    public List<Token> parse(Tokenizer tokenizer) {
        try {
            process(tokenizer);
            //处理尾部为自定义变量的情况
            if (tokenizer.lower() < tokenizer.pos()) {
                tokens.add(new Token(TokenType.INF, tokenizer.str(tokenizer.pos() - 1)));
            }
            if (!exps.isEmpty()) {
                throwSyntaxException(tokenizer);
            }
        } finally {
            exps.empty();
        }
        return tokens;
    }

    private void process(Tokenizer tokenizer) {
        while (tokenizer.hasNext()) {
            //职责链模式
            if (parseWhitespace(tokenizer)) {
            } else if (parseOP(tokenizer)) {
            } else if (parseCMP(tokenizer)) {
            } else if (parseExpress(tokenizer)) {
            } else {
                //自定义自变量
                tokenizer.move();
            }
        }
    }

    /**
     * 解析运算符
     *
     * @param tokenizer
     */
    private boolean parseOP(Tokenizer tokenizer) {
        char ch = tokenizer.current();
        TokenType type = null;
        switch (ch) {
            case Symbol.ADD:
                type = TokenType.ADD;
                break;
            case Symbol.SUB:
                type = TokenType.SUB;
                break;
            case Symbol.MULT:
                type = TokenType.MULT;
                break;
            case Symbol.DIVIDE:
                type = TokenType.DIVIDE;
                break;
        }
        if (type == null) {
            return false;
        }
        if (tokenizer.pos() > tokenizer.lower()) {
            //前面有文本内容
            parseInfOrLogic(tokenizer);
        }
        //左语法检查
        checkLeft(tokenizer);
        //右语法检查
        checkOpRight(tokenizer);
        tokens.add(new Token(type, Character.toString(ch)));
        //窗口滑动
        tokenizer.move();
        tokenizer.lower(tokenizer.pos());
        return true;
    }

    private void checkLeft(Tokenizer tokenizer) {
        boolean error = false;
        if (tokens.size() == 0) {
            error = true;
        }
        if (!error) {
            Token left = this.tokens.get(this.tokens.size() - 1);
            if (!TokenType.INF.equals(left.type()) && !TokenType.RIGHT_BRACKET.equals(left.type())) {
                error = false;
            }
        }
        if (error) {
            throwSyntaxException(tokenizer);
        }
    }

    private void checkOpRight(Tokenizer tokenizer) {
        Character right = tokenizer.nextChar();
        if (right == null) {
            throwSyntaxException(tokenizer);
        }
    }

    public List<Token> tokens() {
        return tokens;
    }

    private boolean parseCMP(Tokenizer tokenizer) {
        char ch = tokenizer.current();
        TokenType type = null;
        switch (ch) {
            case '>':
                type = TokenType.GT;
                break;
            case '<':
                type = TokenType.LT;
                break;
            case '=':
                type = TokenType.EQ;
                break;
            case '!':
                type = TokenType.NEQ;
                break;
            default:
                break;
        }
        if (type == null) {
            return false;
        }
        if (tokenizer.pos() > tokenizer.lower()) {
            //前面有文本内容
            parseInfOrLogic(tokenizer);
        }
        //左侧语法检查
        checkLeft(tokenizer);
        //右侧语法检查
        Character right = tokenizer.next();
        if (TokenType.GT.equals(type) || TokenType.LT.equals(type)) {
            if (right == null || Character.isWhitespace(right) || !right.equals(Symbol.EQ)) {
                this.tokens.add(new Token(type, Character.toString(ch)));
                tokenizer.move();
                tokenizer.lower(tokenizer.pos());
            } else {
                this.tokens.add(new Token(TokenType.GT.equals(type) ? TokenType.GTE : TokenType.LTE, tokenizer.str(tokenizer.pos() + 1)));
                tokenizer.move(2);
                tokenizer.lower(tokenizer.pos());
            }
        } else {
            if (right == null || Character.isWhitespace(right) || !right.equals(Symbol.EQ)) {
                throwSyntaxException(tokenizer);
            }
            this.tokens.add(new Token(type, tokenizer.str(tokenizer.pos() + 1)));
            tokenizer.move(2);
            tokenizer.lower(tokenizer.pos());
        }
        return true;
    }

    private void parseInfOrLogic(Tokenizer tokenizer) {
        String idf = tokenizer.str(tokenizer.pos() - 1);
        TokenType type = TokenType.INF;
        switch (idf) {
            case Symbol.AND:
                type = TokenType.AND;
                break;
            case Symbol.OR:
                type = TokenType.OR;
                break;
            case Symbol.NOT:
                type = TokenType.NOT;
                break;
        }
        //左侧语法检查
        if (TokenType.INF.equals(type)) {
            if (tokens.size() > 0) {
                Token token = tokens.get(tokens.size() - 1);
                if (TokenType.INF.equals(token.type()) || TokenType.RIGHT_BRACKET.equals(token.type())) {
                    throwSyntaxException(tokenizer);
                }
            }
        } else {
            if (tokens.size() == 0 && type != TokenType.NOT) {
                throwSyntaxException(tokenizer);
            } else if (tokens.size() > 1) {
                Token token = tokens.get(tokens.size() - 1);
                if (type == TokenType.NOT) {
                    if (!(TokenType.INF.equals(token.type()) || TokenType.AND.equals(token.type()) || TokenType.OR.equals(token.type()))) {
                        throwSyntaxException(tokenizer);
                    }
                } else if (!TokenType.INF.equals(token.type()) && !TokenType.RIGHT_BRACKET.equals(token.type())) {
                    throwSyntaxException(tokenizer);
                }
            }
        }
        tokenizer.lower(tokenizer.pos());
        this.tokens.add(new Token(type, idf));
    }

    private boolean parseExpress(Tokenizer tokenizer) {
        char ch = tokenizer.current();
        switch (ch) {
            case '(':
                if (tokenizer.pos() > tokenizer.lower()) {
                    //前面有文本内容
                    parseInfOrLogic(tokenizer);
                }
                //左侧语法检查
                if (this.tokens.size() > 0) {
                    Token token = this.tokens.get(this.tokens.size() - 1);
                    if (TokenType.INF.equals(token.type())) {
                        throwSyntaxException(tokenizer);
                    }
                }
                //右侧语法检查
                Character next = tokenizer.nextChar();
                if (next == null || next.equals(Symbol.RIGHT_BRACKET)) {
                    throwSyntaxException(tokenizer);
                }
                //添加token
                tokens.add(new Token(TokenType.LEFT_BRACKET, tokenizer.str(tokenizer.pos())));
                tokenizer.move();
                tokenizer.lower(tokenizer.pos());
                exps.push(Symbol.LEFT_BRACKET);
                //子表达式解析
                //process(tokenizer);
                break;
            case ')':
                //配对检查
                if (exps.isEmpty()) {
                    throwSyntaxException(tokenizer);
                }
                exps.pop();
                if (tokenizer.pos() > tokenizer.lower()) {
                    //前面有文本内容
                    parseInfOrLogic(tokenizer);
                }
                Token token = tokens.get(tokens.size() - 1);
                if (!TokenType.INF.equals(token.type()) && !TokenType.RIGHT_BRACKET.equals(token.type())) {
                    throwSyntaxException(tokenizer);
                }
                tokens.add(new Token(TokenType.RIGHT_BRACKET, tokenizer.str(tokenizer.pos())));
                tokenizer.move();
                tokenizer.lower(tokenizer.pos());
                break;
            default:
                return false;
        }
        return true;
    }

    private boolean parseWhitespace(Tokenizer tokenizer) {
        char ch = tokenizer.current();
        if (Character.isWhitespace(ch)) {
            if (tokenizer.lower() < tokenizer.pos()) {
                //判定为分隔符
                //前面有文本内容
                parseInfOrLogic(tokenizer);
            }
            tokenizer.move();
            tokenizer.lower(tokenizer.pos());
            return true;
        }
        return false;
    }


    private void throwSyntaxException(Tokenizer tokenizer) {
        throw new SyntaxException("the expression " + tokenizer.expr() + " has an syntax error at (" + tokenizer.pos() + "," + tokenizer.current() + ")");
    }

    public static void printTokens(List<Token> tokens) {
        for (Token token : tokens) {
            System.out.println(token);
        }
    }

}
