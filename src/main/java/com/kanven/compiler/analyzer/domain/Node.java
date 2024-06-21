package com.kanven.compiler.analyzer.domain;

public class Node<T> {

    private T value;

    private Node<T> left;

    private Node<T> right;

    public Node() {

    }

    public void value(T value) {
        this.value = value;
    }

    public Node(T value) {
        this.value = value;
    }

    public Node(T value, Node<T> left, Node<T> right) {
        this(value);
        this.left = left;
        this.right = right;
    }

    public void left(Node<T> node) {
        this.left = node;
    }

    public void right(Node<T> node) {
        this.right = node;
    }

    public T value() {
        return this.value;
    }

    public Node<T> left() {
        return this.left;
    }

    public Node<T> right() {
        return this.right;
    }

    @Override
    public String toString() {
        return "{\"value\"" + value + ", \"left\":" + (left == null ? "" : left.value) + ",\"right\":" + (right == null ? "" : right.value) + "}";
    }
}
