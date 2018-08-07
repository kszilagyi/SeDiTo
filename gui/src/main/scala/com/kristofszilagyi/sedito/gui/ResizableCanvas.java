package com.kristofszilagyi.sedito.gui;

import javafx.scene.canvas.Canvas;

// FROM: https://dlsc.com/2014/04/10/javafx-tip-1-resizable-canvas/
final class ResizableCanvas extends Canvas {
    @Override
    public boolean isResizable() {
        return true;
    }

    @Override
    public void resize(double width, double height) {
        setWidth(width);
        setHeight(height);
    }

    @Override
    public double minHeight(double width) { return 0; }
    @Override
    public double minWidth(double height) { return 0; }

    @Override
    public double maxHeight(double width) { return 10000; }
    @Override
    public double maxWidth(double height) { return 10000; }
}

