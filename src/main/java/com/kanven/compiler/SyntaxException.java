package com.kanven.compiler;

public class SyntaxException extends RuntimeException {

    public SyntaxException() {
        super();
    }

    public SyntaxException(String message) {
        super(message);
    }

    public SyntaxException(String message, Throwable e) {
        super(message, e);
    }

    public SyntaxException(Throwable e) {
        super(e);
    }

}
