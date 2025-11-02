package com.example.GUIA3;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.geometry.Pos;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.StackPane;

import java.util.Objects;

    /**
     * The TicTacToeMainMenu class is a JavaFX application that creates a GUI for configuring a Tic-Tac-Toe game,
     * allowing players to select whether each player is human or AI, adjust AI difficulty, choose power-ups for
     * each player, and launch the game with the selected settings.
     *
     * @author 643270vi Ventzi Ivanov
     */
public class TicTacToeMainMenu extends Application {

    private boolean isPlayer1Human = false;
    private boolean isPlayer2Human = false;
    private Slider player1Slider;
    private Slider player2Slider;
    private HBox player1PowerupBox;
    private HBox player2PowerupBox;
    private Label player1DifficultyLabel;
    private Label player2DifficultyLabel;

    private String selectedPowerupPlayer1 = null;
    private String selectedPowerupPlayer2 = null;

    /**
     * The main entry point for the JavaFX application.
     * Initializes and displays the Tic-Tac-Toe main menu.
     *
     * @param primaryStage the primary stage for this application.
     */
    @Override
    public void start(Stage primaryStage) {

        Image computerIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/computer.png")).toExternalForm());
        Image humanIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/human.png")).toExternalForm());

        Image tileSwapperIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/swap.png")).toExternalForm());
        Image tileReserverIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/reserve.png")).toExternalForm());
        Image inverterIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/inverter.png")).toExternalForm());
        Image tileRemoverIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/remover.png")).toExternalForm());

        Label titleLabel = new Label("Tic-Tac-Toe");
        titleLabel.setFont(new Font("Arial", 40));
        titleLabel.setStyle(
                "-fx-text-fill: linear-gradient(cyan, #00FFFF);" +
                        "-fx-effect: dropshadow(gaussian, #00FFFF, 20, 0, 0, 0);"
        );

        Label player1Label = new Label("Player 1");
        player1Label.setFont(new Font("Arial", 20));
        player1Label.setStyle(
                "-fx-text-fill: linear-gradient(cyan, #00FFFF);" +
                        "-fx-effect: dropshadow(gaussian, #00FFFF, 10, 0, 0, 0);"
        );

        ImageView player1IconView = new ImageView(computerIcon);
        player1IconView.setFitWidth(50);
        player1IconView.setFitHeight(50);
        Button player1Button = new Button("", player1IconView);
        player1Button.setStyle(
                "-fx-background-color: black;" +
                        "-fx-border-color: cyan;" +
                        "-fx-border-width: 3;" +
                        "-fx-effect: dropshadow(gaussian, cyan, 10, 0.5, 0, 0);"
        );

        player1Slider = createDifficultySlider();
        player1Slider.setVisible(!isPlayer1Human);

        player1DifficultyLabel = new Label("Difficulty");
        player1DifficultyLabel.setFont(new Font("Arial", 16));
        player1DifficultyLabel.setStyle("-fx-text-fill: linear-gradient(cyan, #00FFFF);");
        player1DifficultyLabel.setVisible(!isPlayer1Human);

        Button tileSwapperButton = createPowerupButton(tileSwapperIcon, "Tile Swapper", "Player1");
        Button tileReserverButton = createPowerupButton(tileReserverIcon, "Tile Reserver", "Player1");
        Button inverterButton = createPowerupButton(inverterIcon, "Inverter", "Player1");
        Button tileRemoverButton = createPowerupButton(tileRemoverIcon, "Tile Remover", "Player1");

        player1PowerupBox = new HBox(10, tileSwapperButton, tileReserverButton, inverterButton, tileRemoverButton);
        player1PowerupBox.setAlignment(Pos.CENTER);
        player1PowerupBox.setVisible(isPlayer1Human);

        StackPane player1PowerupSliderPane = new StackPane(player1Slider, player1PowerupBox);

        player1Button.setOnAction(e -> {
            isPlayer1Human = !isPlayer1Human;
            if (isPlayer1Human) {
                player1IconView.setImage(humanIcon);
                player1PowerupBox.setVisible(true);
                player1Slider.setVisible(false);
                player1DifficultyLabel.setVisible(false);
            } else {
                player1IconView.setImage(computerIcon);
                player1PowerupBox.setVisible(false);
                player1Slider.setVisible(true);
                player1DifficultyLabel.setVisible(true);
            }
        });

        Label player2Label = new Label("Player 2");
        player2Label.setFont(new Font("Arial", 20));
        player2Label.setStyle(
                "-fx-text-fill: linear-gradient(magenta, #FF00FF);" +
                        "-fx-effect: dropshadow(gaussian, #FF00FF, 10, 0, 0, 0);"
        );

        ImageView player2IconView = new ImageView(computerIcon);
        player2IconView.setFitWidth(50);
        player2IconView.setFitHeight(50);
        Button player2Button = new Button("", player2IconView);
        player2Button.setStyle(
                "-fx-background-color: black;" +
                        "-fx-border-color: magenta;" +
                        "-fx-border-width: 3;" +
                        "-fx-effect: dropshadow(gaussian, magenta, 10, 0.5, 0, 0);"
        );

        player2Slider = createDifficultySlider();
        player2Slider.setVisible(!isPlayer2Human);

        player2DifficultyLabel = new Label("Difficulty");
        player2DifficultyLabel.setFont(new Font("Arial", 16));
        player2DifficultyLabel.setStyle("-fx-text-fill: linear-gradient(magenta, #FF00FF);");
        player2DifficultyLabel.setVisible(!isPlayer2Human);

        Button tileSwapperButton2 = createPowerupButton(tileSwapperIcon, "Tile Swapper", "Player2");
        Button tileReserverButton2 = createPowerupButton(tileReserverIcon, "Tile Reserver", "Player2");
        Button inverterButton2 = createPowerupButton(inverterIcon, "Inverter", "Player2");
        Button tileRemoverButton2 = createPowerupButton(tileRemoverIcon, "Tile Remover", "Player2");

        player2PowerupBox = new HBox(10, tileSwapperButton2, tileReserverButton2, inverterButton2, tileRemoverButton2);
        player2PowerupBox.setAlignment(Pos.CENTER);
        player2PowerupBox.setVisible(isPlayer2Human);

        StackPane player2PowerupSliderPane = new StackPane(player2Slider, player2PowerupBox);

        player2Button.setOnAction(e -> {
            isPlayer2Human = !isPlayer2Human;
            if (isPlayer2Human) {
                player2IconView.setImage(humanIcon);
                player2PowerupBox.setVisible(true);
                player2Slider.setVisible(false);
                player2DifficultyLabel.setVisible(false);
            } else {
                player2IconView.setImage(computerIcon);
                player2PowerupBox.setVisible(false);
                player2Slider.setVisible(true);
                player2DifficultyLabel.setVisible(true);
            }
        });

        Button confirmButton = new Button("Confirm");
        confirmButton.setFont(new Font("Arial", 20));
        confirmButton.setStyle(
                "-fx-background-color: black;" +
                        "-fx-text-fill: linear-gradient(cyan, #00FFFF, magenta, #FF00FF);" +
                        "-fx-border-color: linear-gradient(cyan, #00FFFF, magenta, #FF00FF);" +
                        "-fx-border-width: 3;" +
                        "-fx-effect: dropshadow(gaussian, #00FFFF, 10, 0.5, 0, 0);"
        );

        confirmButton.setOnAction(e -> {
            int player1Difficulty = (int) player1Slider.getValue();
            int player2Difficulty = (int) player2Slider.getValue();

            startTicTacToeBoard(primaryStage, isPlayer1Human, isPlayer2Human, player1Difficulty, player2Difficulty, selectedPowerupPlayer1, selectedPowerupPlayer2);
        });

        VBox player1Box = new VBox(5, player1DifficultyLabel, player1PowerupSliderPane);
        HBox player1HBox = new HBox(20, player1Button, player1Box);
        player1HBox.setAlignment(Pos.CENTER);

        VBox player2Box = new VBox(5, player2DifficultyLabel, player2PowerupSliderPane);
        HBox player2HBox = new HBox(20, player2Button, player2Box);
        player2HBox.setAlignment(Pos.CENTER);

        VBox layout = new VBox(30);
        layout.getChildren().addAll(titleLabel, player1Label, player1HBox, player2Label, player2HBox, confirmButton);
        layout.setAlignment(Pos.CENTER);
        layout.setStyle(
                "-fx-background-color: radial-gradient(center 50% 50%, radius 100%, black, #222);"
        );

        Scene scene = new Scene(layout, 600, 600);
        primaryStage.setScene(scene);
        primaryStage.setTitle("Neon Tic-Tac-Toe");
        primaryStage.show();
    }


    /**
     * Creates a difficulty slider for the players.
     * The slider ranges from 0 to 6 with visible tick marks.
     *
     * @return a Slider object configured for difficulty selection.
     */
    private Slider createDifficultySlider() {
        Slider slider = new Slider(0, 6, 1);
        slider.setShowTickLabels(true);
        slider.setShowTickMarks(true);
        slider.setMajorTickUnit(1);
        slider.setMinorTickCount(0);
        slider.setSnapToTicks(true);
        slider.setStyle(
                "-fx-control-inner-background: black;" +
                        "-fx-border-color: linear-gradient(cyan, magenta);" +
                        "-fx-effect: dropshadow(gaussian, cyan, 10, 0.5, 0, 0);"
        );
        return slider;
    }

    /**
     * Creates a power-up button with an image and tooltip.
     * Each button represents a different power-up (Tile Swapper, Reserver, Inverter, etc.).
     *
     * @param icon        the Image to display on the button.
     * @param tooltipText the tooltip text to display on hover.
     * @param player      the player associated with the button ("Player1" or "Player2").
     * @return a Button object configured with the power-up icon and tooltip.
     */
    private Button createPowerupButton(Image icon, String tooltipText, String player) {
        ImageView imageView = new ImageView(icon);
        imageView.setFitWidth(40);
        imageView.setFitHeight(40);

        Button button = new Button("", imageView);
        button.setStyle(
                "-fx-background-color: black;" +
                        "-fx-border-color: cyan;" +
                        "-fx-border-width: 2;" +
                        "-fx-shape: 'M 100, 100 m -50, 0 a 50,50 0 1,0 100,0 a 50,50 0 1,0 -100,0';" +
                        "-fx-effect: dropshadow(gaussian, cyan, 10, 0, 0, 0);"
        );
        Tooltip tooltip = new Tooltip(tooltipText);
        Tooltip.install(button, tooltip);

        button.setOnAction(e -> {
            if (player.equals("Player1")) {
                selectPowerupPlayer1(tooltipText, button);
            } else {
                selectPowerupPlayer2(tooltipText, button);
            }
        });
        return button;
    }

    /**
     * Handles power-up selection for Player 1.
     * Sets the selected power-up and highlights the button.
     *
     * @param powerup        the power-up selected by Player 1.
     * @param selectedButton the Button representing the selected power-up.
     */
    private void selectPowerupPlayer1(String powerup, Button selectedButton) {
        selectedPowerupPlayer1 = powerup;
        highlightSelectedButton(selectedButton, "Player1");
    }

    /**
     * Handles power-up selection for Player 2.
     * Sets the selected power-up and highlights the button.
     *
     * @param powerup        the power-up selected by Player 2.
     * @param selectedButton the Button representing the selected power-up.
     */
    private void selectPowerupPlayer2(String powerup, Button selectedButton) {
        selectedPowerupPlayer2 = powerup;
        highlightSelectedButton(selectedButton, "Player2");
    }

    /**
     * Highlights the selected power-up button for the given player.
     * Updates the button style to indicate which power-up is active.
     *
     * @param selectedButton the Button to highlight.
     * @param player         the player ("Player1" or "Player2") for whom the power-up is selected.
     */
    private void highlightSelectedButton(Button selectedButton, String player) {
        HBox powerupBox = player.equals("Player1") ? player1PowerupBox : player2PowerupBox;

        for (var button : powerupBox.getChildren()) {
            button.setStyle(
                    "-fx-background-color: black;" +
                            "-fx-border-color: cyan;" +
                            "-fx-border-width: 2;" +
                            "-fx-shape: 'M 100, 100 m -50, 0 a 50,50 0 1,0 100,0 a 50,50 0 1,0 -100,0';" +
                            "-fx-effect: dropshadow(gaussian, cyan, 10, 0, 0, 0);"
            );
        }

        selectedButton.setStyle(
                "-fx-background-color: black;" +
                        "-fx-border-color: magenta;" +
                        "-fx-border-width: 3;" +
                        "-fx-shape: 'M 100, 100 m -50, 0 a 50,50 0 1,0 100,0 a 50,50 0 1,0 -100,0';" +
                        "-fx-effect: dropshadow(gaussian, magenta, 10, 0, 0, 0);"
        );
    }

    /**
     * Starts the Tic-Tac-Toe game board after players have been configured.
     * Transfers settings such as player types, difficulties, and selected power-ups to the game board.
     *
     * @param primaryStage   the primary stage to display the Tic-Tac-Toe game.
     * @param isPlayer1Human whether Player 1 is human.
     * @param isPlayer2Human whether Player 2 is human.
     * @param player1Depth   the difficulty level for Player 1 (if AI).
     * @param player2Depth   the difficulty level for Player 2 (if AI).
     * @param powerup1       the selected power-up for Player 1.
     * @param powerup2       the selected power-up for Player 2.
     */
    private void startTicTacToeBoard(Stage primaryStage, boolean isPlayer1Human, boolean isPlayer2Human, int player1Depth, int player2Depth, String powerup1, String powerup2) {
        TicTacToeBoard ticTacToeBoard = new TicTacToeBoard(isPlayer1Human, isPlayer2Human, player1Depth, player2Depth, powerup1, powerup2);
        ticTacToeBoard.start(primaryStage);
    }

    /**
     * The main method that launches the application.
     * @param args the command-line arguments (if any).
     */
    public static void main(String[] args) {
        launch(args);
    }
}
