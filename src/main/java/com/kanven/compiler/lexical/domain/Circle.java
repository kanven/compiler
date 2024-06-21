package com.kanven.compiler.lexical.domain;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class Circle {

    private static final char END_SYMBOL = '@';

    public static Circle end = new Circle(END_SYMBOL);

    private char ch;

    private Set<Circle> children = new HashSet<>(0);

    public Circle() {

    }

    public Circle(char ch) {
        this.ch = ch;
    }

    public Circle match(char ch) {
        for (Circle circle : children) {
            if (circle.ch == ch) {
                return circle;
            }
        }
        return null;
    }

    public void child(Circle child) {
        if (child != null) {
            children.add(child);
        }
    }

    public Set<Circle> children() {
        return this.children;
    }

    public boolean hasEnd() {
        if (children.contains(end)) {
            return true;
        }
        return false;
    }

    public boolean isEnd() {
        if (children.size() == 1) {
            for (Circle child : children) {
                if (child.ch == END_SYMBOL) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Circle circle = (Circle) o;
        return ch == circle.ch;
    }

    @Override
    public int hashCode() {
        return Objects.hash(ch);
    }

    @Override
    public String toString() {
        return "{\"ch\":'" + ch + "'}";
    }
}
