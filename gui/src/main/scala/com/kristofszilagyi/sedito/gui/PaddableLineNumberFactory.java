package com.kristofszilagyi.sedito.gui;

import java.util.function.IntFunction;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;

import org.fxmisc.richtext.GenericStyledArea;
import org.reactfx.collection.LiveList;
import org.reactfx.value.Val;

/**
 * Graphic factory that produces labels containing line numbers.
 * To customize appearance, use {@code .lineno} style class in CSS stylesheets.
 */
public class PaddableLineNumberFactory implements IntFunction<Node> {

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
        return new PaddableLineNumberFactory(area, format);
    }

    private final Val<Integer> nParagraphs;
    private final IntFunction<String> format;

    private PaddableLineNumberFactory(
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
        lineNo.setPadding(new Insets(10, 10, 10, 10));
        lineNo.setAlignment(Pos.TOP_RIGHT);
        lineNo.getStyleClass().add("lineno");

        // bind label's text to a Val that stops observing area's paragraphs
        // when lineNo is removed from scene
        lineNo.textProperty().bind(formatted.conditionOnShowing(lineNo));

        // or try StackPane if Pane doesn't work
        //StackPane p = new StackPane();
       // p.getChildren().add(lineNo);
//        Insets padding = new Insets(10, 10, 10, 10);
//        p.setPadding(padding);
        //lineNo.setStyle("-fx-padding: 30 0 30 0;");
        return lineNo;
    }

    private String format(int x, int max) {
        int digits = (int) Math.floor(Math.log10(max)) + 1;
        return String.format(format.apply(digits), x);
    }
}
