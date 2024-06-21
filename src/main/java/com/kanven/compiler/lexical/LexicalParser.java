package com.kanven.compiler.lexical;

import com.kanven.compiler.SyntaxException;
import com.kanven.compiler.lexical.domain.Token;
import com.kanven.compiler.lexical.grammar.Grammar;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;

/**
 * 通用词法解析器
 */
public class LexicalParser {

    private Grammar grammar;

    public LexicalParser(Grammar grammar) {
        this.grammar = grammar;
    }


    public TokenStream parse(String content) {
        if (StringUtils.isBlank(content)) {
            throw new NullPointerException("content is empty!");
        }
        List<Token> tokens = new ArrayList<>(0);
        Tokenizer tokenizer = new Tokenizer(content);
        parse(tokenizer, tokens);
        return new TokenStream(tokens);
    }

    private void parse(Tokenizer tokenizer, List<Token> tokens) {
        while (tokenizer.hasNext()) {
            tokenizer.skipWhitespace();
            //终止符号匹配（TODO 操作符和其他文案串连形式暂未考虑）
            Token matched = grammar.graph().match(tokenizer);
            if (matched != null) {
                tokens.add(matched);
            } else {
                char ch = tokenizer.current();
                Token token;
                if (Character.isWhitespace(ch)) {
                    if (tokenizer.lower() != tokenizer.pos()) {
                        int end = tokenizer.pos() - 1;
                        String str = tokenizer.str(end);
                        token = new Token(str, tokenizer.lower(), end);
                        tokenizer.move();
                        tokenizer.lower(tokenizer.pos());
                    } else {
                        continue;
                    }
                } else {
                    do {
                        if (grammar.isTerminator(Character.toString(ch))) {
                            break;
                        }
                        if (tokenizer.hasNext()) {
                            tokenizer.move();
                            ch = tokenizer.current();
                        } else {
                            break;
                        }
                    } while (!Character.isWhitespace(ch));
                    int end = tokenizer.pos() - 1;
                    String str = tokenizer.str(end);
                    token = new Token(str, tokenizer.lower(), end);
                }
                final AtomicBoolean success = new AtomicBoolean(false);
                grammar.identifier().forEach((non, pattern) -> {
                    Matcher matcher = pattern.matcher(token.symbol());
                    if (matcher.matches()) {
                        tokens.add(token);
                        success.set(true);
                    }
                });
                if (!success.get()) {
                    throw new SyntaxException("an error occur,the token is:" + token);
                }
                tokenizer.lower(tokenizer.pos());
            }
            tokenizer.skipWhitespace();
        }
    }


    public static void main(String[] args) throws Exception {
        String exp = "(a + e) * t > f + g and not (b or c) ";
        Grammar grammar = new Grammar("grammar.txt");
        LexicalParser parser = new LexicalParser(grammar);
        TokenStream stream = parser.parse(exp);
        while (stream.hasToken()) {
            Token token = stream.current();
            System.out.println(token);
            stream.move();
        }
    }


}
