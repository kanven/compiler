package com.kanven.compiler.lexical.domain;

import com.kanven.compiler.lexical.Tokenizer;

/**
 * 语法自定义变量
 */
public class Graph {

    private final Circle start = new Circle();

    public Graph() {

    }

    public void build(String identify) {
        Circle circle = start;
        char[] chs = identify.toCharArray();
        for (char ch : chs) {
            Circle c = circle.match(ch);
            if (c != null) {
                circle = c;
            } else {
                c = new Circle(ch);
                circle.child(c);
                circle = c;
            }
        }
        circle.child(Circle.end);
    }

    private Token buildToken(Tokenizer tokenizer) {
        int end = tokenizer.pos() - 1;
        String str = tokenizer.str(end);
        Token token = new Token(str, tokenizer.lower(), end);
        tokenizer.lower(tokenizer.pos());
        return token;
    }

    public Token match(Tokenizer tokenizer) {
        tokenizer.skipWhitespace();
        if (!tokenizer.hasNext()) {
            //字符流到达末尾
            return null;
        }
        Circle circle = start;
        if (circle.isEnd()) {
            //无匹配项
            return null;
        }
        char ch;
        while (true) {
            ch = tokenizer.current();
            Circle current = circle.match(ch);
            if (current != null) {
                //匹配成功
                if (tokenizer.hasNext()) {
                    //移动到下一位
                    tokenizer.move();
                    if (Character.isWhitespace(tokenizer.current())) {
                        //到达下一个字符串
                        if (current.hasEnd()) {
                            //完全匹配
                            return buildToken(tokenizer);
                        }
                        //匹配失败
                        return null;
                    }
                    //还在同一字符串
                    if (current.isEnd()) {
                        //匹配到达末尾,TODO 可能出现关键词前缀的词
                        return buildToken(tokenizer);
                    }
                    circle = current;
                } else {
                    //字符流到末尾
                    if (current.hasEnd()) {
                        //完全匹配
                        return buildToken(tokenizer);
                    }
                    //匹配失败
                    return null;
                }
            } else {
                //长、短字符串匹配场景处理
                if (circle.hasEnd()) {
                    //前一个字符完全匹配
                    return buildToken(tokenizer);
                }
                //没有匹配项
                return null;
            }
        }
    }

}
