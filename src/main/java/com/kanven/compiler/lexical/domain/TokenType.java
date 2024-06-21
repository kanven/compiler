package com.kanven.compiler.lexical.domain;

public enum TokenType {

    /**
     * 字面量
     */
    INF(0),
    /**
     * 运算符（+,-,*,\）
     */
    ADD(1),
    SUB(1),
    MULT(2),
    DIVIDE(2),
    /**
     * 逻辑运算符号
     */
    AND(3),
    OR(4),
    NOT(5),
    /**
     * 比较运算符（>,>=,<,<=,!=）
     */
    GT(6),
    GTE(6),
    LT(6),
    LTE(6),
    EQ(6),
    NEQ(6),
    /**
     * 括号()
     */
    LEFT_BRACKET(7),

    RIGHT_BRACKET(7);

    private int priority;

    TokenType(int priority) {
        this.priority = priority;
    }

    public boolean matchType(Token token) {
        return this == token.type();
    }

    public static boolean isOp(Token token) {
        TokenType type = token.type();
        return type == ADD || type == SUB || type == MULT || type == DIVIDE;
    }

    public static boolean isCmp(Token token) {
        TokenType type = token.type();
        return type == GT || type == GTE || type == LT || type == LTE || type == EQ || type == NEQ;
    }


    public static boolean isLogic(Token token) {
        TokenType type = token.type();
        return type == AND || type == OR || type == NOT;
    }

    public static boolean isInf(Token token) {
        TokenType type = token.type();
        return type == INF;
    }

}
