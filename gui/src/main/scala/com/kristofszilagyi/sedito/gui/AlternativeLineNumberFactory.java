package com.kristofszilagyi.sedito.gui;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import org.fxmisc.richtext.GenericStyledArea;
import org.reactfx.collection.LiveList;
import org.reactfx.value.Val;

import java.util.function.IntFunction;

public class AlternativeLineNumberFactory implements IntFunction<Node> {

    private static final Insets DEFAULT_INSETS = new Insets(0.0, 5.0, 0.0, 5.0);
    private static final Paint DEFAULT_TEXT_FILL = Color.web("#666");
    private static final Font DEFAULT_FONT =
            Font.font("monospace", FontPosture.ITALIC, 13);
    private static final Background DEFAULT_BACKGROUND =
            new Background(new BackgroundFill(Color.web("#ddd"), null, null));

    public static IntFunction<Node> get(GenericStyledArea<?, ?, ?> area) {
        return get(area, digits -> "%1$" + digits + "s");
    }

    public static IntFunction<Node> get(
            GenericStyledArea<?, ?, ?> area,
            IntFunction<String> format) {
        return new AlternativeLineNumberFactory(area, format);
    }

    private final Val<Integer> nParagraphs;
    private final IntFunction<String> format;

    private AlternativeLineNumberFactory(
            GenericStyledArea<?, ?, ?> area,
            IntFunction<String> format) {
        nParagraphs = LiveList.sizeOf(area.getParagraphs());
        this.format = format;
    }

    @Override
    public Node apply(int idx) {
        Val<String> formatted = nParagraphs.map(n -> format(idx+1, n));

        Label lineNo = new Label();
        lineNo.setFont(DEFAULT_FONT);
        lineNo.setBackground(DEFAULT_BACKGROUND);
        lineNo.setTextFill(DEFAULT_TEXT_FILL);
        lineNo.setPadding(DEFAULT_INSETS);
        lineNo.setAlignment(Pos.TOP_RIGHT);
        lineNo.getStyleClass().add("lineno");

        // bind label's text to a Val that stops observing area's paragraphs
        // when lineNo is removed from scene
        lineNo.textProperty().bind(formatted.conditionOnShowing(lineNo));

        StackPane r = new StackPane();
        r.getChildren().add(lineNo);
        int height = 110;
        double minHeight = height;
        r.setMinHeight(minHeight);
        r.setPrefHeight(minHeight);

        return r;
    }

    private String format(int x, int max) {
        int digits = (int) Math.floor(Math.log10(max)) + 1;
        return String.format(format.apply(digits), x);
    }

}
