package com.kanven.compiler.lexical.grammar;


import com.kanven.compiler.lexical.domain.Graph;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.regex.Pattern;

public class Grammar {

    private static final String Vertical = "|";

    private Graph graph = new Graph();

    /**
     * 终结符
     */
    private List<String> terminator = new ArrayList<>(0);

    /**
     * 非终止符
     */
    private List<String> nonterminal = new ArrayList<>(0);

    /**
     * 字面量
     */
    private Map<String, Pattern> identifier = new HashMap<>(0);

    /**
     * 正排索引
     */
    private Map<String, Statement> statements = new HashMap<>();

    /**
     * 倒排索引
     */
    private Map<String, List<Index>> indices = new HashMap<>();


    public Grammar(String filename) throws IOException {
        load(filename);
        buildNonterminalAndIdentifier();
        buildTerminator();
        buildIndex();
    }

    private void load(String filename) throws IOException {
        BufferedReader reader = null;
        try {
            Class<Grammar> clazz = Grammar.class;
            ClassLoader loader = clazz.getClassLoader();
            InputStream input = loader.getResourceAsStream(filename);
            reader = new BufferedReader(new InputStreamReader(input));
            String line = null;
            Statement statement = null;
            while ((line = reader.readLine()) != null) {
                List<String> items = parseLine(line);
                if (items.isEmpty()) {
                    continue;
                }
                int gap = 0;
                if (items.contains("::=")) {
                    //另外一个statement
                    String non = items.get(0);
                    statement = new Statement(non);
                    statements.put(non, statement);
                    gap = 2;
                } else if (items.contains(Vertical)) {
                    gap = 1;
                }
                List<String> seqs = new ArrayList<>();
                int idx = 0;
                while ((idx + gap) < items.size()) {
                    String item = items.get(idx + gap);
                    seqs.add(item);
                    idx++;
                }
                statement.expressions.add(seqs);
            }
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
    }


    private List<String> parseLine(String line) {
        List<String> items = new ArrayList<>();
        if (!StringUtils.isBlank(line)) {
            int start = 0;
            char[] chs = line.toCharArray();
            int len = chs.length;
            for (int i = 0; i < len; i++) {
                char ch = chs[i];
                if (Character.isWhitespace(ch)) {
                    if (i - start > 0) {
                        items.add(line.substring(start, i));
                    }
                    start = i + 1;
                } else {
                    if (Vertical.equals(Character.toString(ch))) {
                        items.add(Vertical);
                        start++;
                    }
                }
            }
            //最后一个字符串且末尾没有空格
            if (start < len) {
                items.add(line.substring(start));
            }
        }
        return items;
    }

    /**
     * 筛选非终结符和字面量
     */
    private void buildNonterminalAndIdentifier() {
        Set<String> nons = statements.keySet();
        for (String non : nons) {
            Statement stat = statements.get(non);
            List<List<String>> expressions = stat.expressions;
            if (expressions.size() == 1) {
                List<String> expression = expressions.get(0);
                if (!expression.contains(non) && expression.size() == 1) {
                    String exp = expression.get(0);
                    if (!exp.startsWith("^")) {
                        exp = "^" + exp;
                    }
                    if (!exp.endsWith("$")) {
                        exp = exp + "$";
                    }
                    Pattern pattern = Pattern.compile(exp);
                    this.identifier.put(non, pattern);
                    continue;
                }
            }
            if (!this.nonterminal.contains(non)) {
                this.nonterminal.add(non);
            }
        }
    }


    private void buildTerminator() {
        for (Statement statement : statements.values()) {
            if (identifier.keySet().contains(statement.non)) {
                continue;
            }
            for (List<String> expression : statement.expressions) {
                for (String item : expression) {
                    if (!nonterminal.contains(item) && !identifier.keySet().contains(item)) {
                        if (terminator.contains(item)) {
                            continue;
                        }
                        terminator.add(item);
                        graph.build(item);
                    }
                }
            }
        }
    }

    private void buildIndex() {
        //构建非终结符倒排索引
        for (Grammar.Statement stat : statements.values()) {
            List<List<String>> expressions = stat.expressions();
            for (int i = 0; i < expressions.size(); i++) {
                List<String> expression = expressions.get(i);
                for (int j = 0; j < expression.size(); j++) {
                    String item = expression.get(j);
                    if (nonterminal.contains(item)) {
                        List<Index> indices = this.indices.computeIfAbsent(item, k -> new ArrayList<>(0));
                        Index index = new Index(stat.non(), i, j);
                        indices.add(index);
                    }
                }
            }
        }
    }


    static class Statement {

        private final String non;

        private List<List<String>> expressions = new ArrayList<>();

        public Statement(String non) {
            this.non = non;
        }

        public List<List<String>> expressions() {
            return this.expressions;
        }

        public String non() {
            return this.non;
        }

    }

    static final class Index {

        private final String non;

        private final int seq;

        private final int idx;

        public Index(String non, int seq, int idx) {
            this.non = non;
            this.seq = seq;
            this.idx = idx;
        }

        public String non() {
            return non;
        }

        public int seq() {
            return seq;
        }

        public int idx() {
            return idx;
        }

    }

    public String start() {
        return this.nonterminal.get(0);
    }

    public Map<String, Statement> statements() {
        return statements;
    }

    public List<String> nonterminal() {
        return this.nonterminal;
    }

    public Map<String, Pattern> identifier() {
        return this.identifier;
    }

    public Map<String, List<Index>> indices() {
        return this.indices;
    }

    public boolean isTerminator(String item) {
        return this.terminator.contains(item);
    }

    public boolean isIdentifier(String item) {
        return this.identifier.keySet().contains(item);
    }

    public Graph graph() {
        return this.graph;
    }


    public static void main(String[] args) throws Exception {
        new Grammar("grammar.txt");
    }

}
