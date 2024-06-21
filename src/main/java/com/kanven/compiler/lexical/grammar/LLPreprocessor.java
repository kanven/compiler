package com.kanven.compiler.lexical.grammar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LLPreprocessor {

    private static final String Nullable = "∈";

    private static final String End = "eof";

    private Map<String, List<String>> first = new HashMap<>();

    private Map<String, List<String>> follow = new HashMap<>();

    private Map<String, Map<String, List<String>>> flow = new HashMap<>();

    private Grammar grammar;

    public LLPreprocessor(Grammar grammar) {
        this.grammar = grammar;
        buildFirst();
        buildFollow();
        buildFlow();
    }

    private void buildFirst() {
        for (String non : grammar.nonterminal()) {
            if (this.first.get(non) == null) {
                parseStatement(non, new ArrayList<>(0));
            }
        }
    }

    private void parseStatement(String non, List<String> terms) {
        Grammar.Statement stat = grammar.statements().get(non);
        List<List<String>> expressions = stat.expressions();
        for (List<String> seq : expressions) {
            String item = seq.get(0);
            if (grammar.nonterminal().contains(item)) {
                //非终结符
                List<String> items = null;
                if (this.first.containsKey(item)) {
                    items = this.first.get(item);
                } else {
                    items = new ArrayList<>(0);
                    parseStatement(item, items);
                }
                if (seq.size() > 1) {
                    items.remove(Nullable);
                }
                addFirst(terms, items);
            } else {
                if (!terms.contains(item)) {
                    terms.add(item);
                }
            }
        }
        this.first.put(non, terms);
    }

    private void addFirst(List<String> terms, List<String> items) {
        for (String item : items) {
            if (terms.contains(item)) {
                continue;
            }
            terms.add(item);
        }
    }

    private void buildFollow() {
        for (String non : grammar.nonterminal()) {
            if (this.follow.get(non) == null) {
                this.follow.put(non, new ArrayList<>(0));
                parseFollow(non, this.follow.get(non));
            }
        }
    }

    private void parseFollow(String non, List<String> follow) {
        if (!follow.contains(End)) {
            follow.add(End);
        }
        List<Grammar.Index> indices = grammar.indices().get(non);
        if (indices == null || indices.isEmpty()) {
            return;
        }
        for (Grammar.Index index : indices) {
            Grammar.Statement statement = grammar.statements().get(index.non());
            List<List<String>> expressions = statement.expressions();
            List<String> expression = expressions.get(index.seq());
            if (index.idx() == expression.size() - 1) {
                fetchFollow(statement.non(), follow);
            } else {
                String next = expression.get(index.idx() + 1);
                if (grammar.nonterminal().contains(next)) {
                    List<String> first = this.first.get(next);
                    if (first != null && !first.isEmpty()) {
                        addFollow(follow, first);
                        if (first.contains(Nullable)) {
                            fetchFollow(statement.non(), follow);
                        }
                    }
                } else {
                    if (Nullable.equals(next) && !follow.contains(End)) {
                        follow.add(End);
                    } else {
                        follow.add(next);
                    }
                }
            }
        }
    }

    private void fetchFollow(String non, List<String> follow) {
        List<String> items = this.follow.get(non);
        if (items == null || items.isEmpty()) {
            items = new ArrayList<>(0);
            parseFollow(non, items);
            this.follow.put(non, items);
        }
        addFollow(follow, items);
        if (follow.contains(Nullable)) {
            follow.remove(Nullable);
        }
    }

    private void addFollow(List<String> follow, List<String> items) {
        if (items == null || items.isEmpty()) {
            return;
        }
        for (String item : items) {
            if (follow.contains(item)) {
                continue;
            }
            follow.add(item);
        }
    }


    private void buildFlow() {
        for (String non : grammar.nonterminal()) {
            flow.put(non, new HashMap<>());
            List<String> first = this.first.get(non);
            Grammar.Statement stat = grammar.statements().get(non);
            List<List<String>> expressions = stat.expressions();
            for (String term : first) {
                for (int i = 0; i < expressions.size(); i++) {
                    List<String> expression = expressions.get(i);
                    String expr = expression.get(0);
                    if (grammar.nonterminal().contains(expr)) {
                        //非终结符
                        List<String> items = this.first.get(expr);
                        if (items.contains(term)) {
                            //匹配其中一个first
                            flow.get(non).put(term, expression);
                        }
                    } else if (term.equals(expr)) {
                        flow.get(non).put(term, expression);
                    }
                }
            }
        }
    }

    public Map<String, Map<String, List<String>>> flow() {
        return this.flow;
    }


}
