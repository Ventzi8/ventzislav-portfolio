package com.example.GUIA3;
import com.example.GUIA3.logic.TicTacToe;
import com.example.GUIA3.logic.MiniMax;
import javafx.application.Platform;

import java.util.*;

import javafx.animation.KeyFrame;
import javafx.animation.PauseTransition;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.util.Duration;
import javafx.geometry.Pos;
import javafx.geometry.Insets;
import javafx.scene.layout.StackPane;

    /**
     * This class implements a 5x5 Tic-Tac-Toe game with additional power-up functionalities
     * (such as tile remover, tile swapper, and tile reserver), allowing for both human and AI players,
     * and includes methods to manage gameplay, AI logic, user interface rendering with JavaFX,
     * win/draw condition checks, and visual updates based on user actions or power-ups, ensuring the game is
     * fully interactive with clear handling of turns and game state transitions.
     *
     * @author 643270vi Ventzi Ivanov
     */
public class TicTacToeBoard extends Application {
    private Button reservedTilePlayer1 = null;
    private Button reservedTilePlayer2 = null;
    private final Map<Button, Integer> reservedTileCountdownPlayer1 = new HashMap<>();
    private final Map<Button, Integer> reservedTileCountdownPlayer2 = new HashMap<>();
    private boolean tileReserverActive = false;
    private final Map<Button, Integer> reservedTileCountdown = new HashMap<>();
    private Button firstSelectedTile = null;
    private Button secondSelectedTile = null;
    private boolean tileSwapperActive = false;
    private final Map<Button, String> originalBorderColors = new HashMap<>();
    private Stage primaryStage;
    private Image xIcon;
    private Image oIcon;
    private boolean isPlayer1Turn = true;
    private boolean gameOver = false;
    public static final String PLAYER_1_COLOR = "cyan";
    public static final String PLAYER_2_COLOR = "magenta";
    private final int player1Depth;
    private final int player2Depth;
    public boolean isPlayer1Human;
    public boolean isPlayer2Human;
    private final String player1Powerup;
    private final String player2Powerup;
    private TicTacToe.Board board = new TicTacToe.Board();
    public GridPane grid;
    private Canvas canvas;
    private HBox buttonBox;
    private boolean player1PowerupUsed = false;
    private boolean player2PowerupUsed = false;
    private Button player1PowerupButton;
    private Button player2PowerupButton;
    private Label endGameLabel;

    /**
     * Constructs a new TicTacToeBoard instance with specified player settings.
     *
     * @param isPlayer1Human  Indicates if Player 1 is human.
     * @param isPlayer2Human  Indicates if Player 2 is human.
     * @param player1Depth    The AI depth level for Player 1.
     * @param player2Depth    The AI depth level for Player 2.
     * @param player1Powerup  The power-up selected by Player 1.
     * @param player2Powerup  The power-up selected by Player 2.
     */
    public TicTacToeBoard(boolean isPlayer1Human, boolean isPlayer2Human, int player1Depth, int player2Depth, String player1Powerup, String player2Powerup) {
        this.isPlayer1Human = isPlayer1Human;
        this.isPlayer2Human = isPlayer2Human;
        this.player1Depth = player1Depth;
        this.player2Depth = player2Depth;
        this.player1Powerup = player1Powerup;
        this.player2Powerup = player2Powerup;
    }

    /**
     * Starts the TicTacToe game by initializing the board, canvas, buttons, and event handlers.
     *
     * @param primaryStage The primary stage for this application.
     */
    @Override
    public void start(Stage primaryStage) {
        this.primaryStage = primaryStage;
        this.xIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/fraudx2.png")).toExternalForm());
        this.oIcon = new Image(Objects.requireNonNull(getClass().getResource("/images/circle.png")).toExternalForm());

        grid = new GridPane();
        grid.setAlignment(Pos.CENTER);
        grid.setPadding(new Insets(20));
        grid.setHgap(10);
        grid.setVgap(10);
        canvas = new Canvas(600, 600);
        GraphicsContext gc = canvas.getGraphicsContext2D();
        canvas.setMouseTransparent(true);

        for (int row = 0; row < 5; row++) {
            for (int col = 0; col < 5; col++) {
                Button cellButton = new Button();
                cellButton.setPrefSize(100, 100);
                setButtonBorder(cellButton, PLAYER_1_COLOR);
                cellButton.setMinSize(100, 100);
                cellButton.setMaxSize(100, 100);
                cellButton.setOnAction(e -> handleCellClick(cellButton, xIcon, oIcon, primaryStage));
                grid.add(cellButton, col, row);
            }
        }

        VBox buttonContainer = new VBox(10);
        buttonContainer.setAlignment(Pos.CENTER);
        buttonContainer.setPadding(new Insets(10));

        HBox powerupButtonBox = new HBox(20);
        powerupButtonBox.setAlignment(Pos.CENTER);

        HBox actionButtonBox = new HBox(20);
        actionButtonBox.setAlignment(Pos.CENTER);
        powerupButtonBox.prefWidthProperty().bind(buttonContainer.widthProperty());
        actionButtonBox.prefWidthProperty().bind(buttonContainer.widthProperty());

        if (player1Powerup != null) {
            createPowerupButton(player1Powerup, "Player 1", true, powerupButtonBox);
        }

        if (player2Powerup != null) {
            createPowerupButton(player2Powerup, "Player 2", false, powerupButtonBox);
        }

        if (!powerupButtonBox.getChildren().isEmpty()) {
            buttonContainer.getChildren().add(powerupButtonBox);
        }

        Button resetButton = new Button("Reset");
        resetButton.setFont(new Font("Arial", 20));
        resetButton.setPadding(new Insets(10));
        resetButton.setStyle(
                "-fx-background-color: black;" +
                        "-fx-text-fill: linear-gradient(cyan, #00FFFF, magenta, #FF00FF);" +
                        "-fx-border-color: linear-gradient(cyan, #00FFFF, magenta, #FF00FF);" +
                        "-fx-border-width: 3;" +
                        "-fx-effect: dropshadow(gaussian, #00FFFF, 10, 0.5, 0, 0);"
        );
        resetButton.setOnAction(e -> {
            resetGame(xIcon, oIcon, primaryStage);
            gc.clearRect(0, 0, canvas.getWidth(), canvas.getHeight());
        });
        actionButtonBox.getChildren().add(resetButton);

        Button mainMenuButton = new Button("Main Menu");
        mainMenuButton.setFont(new Font("Arial", 20));
        mainMenuButton.setPadding(new Insets(10));
        mainMenuButton.setStyle(
                "-fx-background-color: black;" +
                        "-fx-text-fill: linear-gradient(cyan, #00FFFF, magenta, #FF00FF);" +
                        "-fx-border-color: linear-gradient(cyan, #00FFFF, magenta, #FF00FF);" +
                        "-fx-border-width: 3;" +
                        "-fx-effect: dropshadow(gaussian, #00FFFF, 10, 0.5, 0, 0);"
        );
        mainMenuButton.setOnAction(e -> {
            TicTacToeMainMenu mainMenu = new TicTacToeMainMenu();
            mainMenu.start(primaryStage);
        });
        actionButtonBox.getChildren().add(mainMenuButton);

        buttonContainer.getChildren().add(actionButtonBox);

        VBox layout = new VBox(30);
        layout.setAlignment(Pos.CENTER);
        layout.setPadding(new Insets(30));
        layout.getChildren().addAll(grid, buttonContainer);

        StackPane root = new StackPane();
        root.setStyle("-fx-background-color: black;");
        root.getChildren().addAll(layout, canvas);

        Scene scene = new Scene(root, 650, 800);
        primaryStage.setScene(scene);
        primaryStage.setTitle("5x5 Neon Tic-Tac-Toe");
        primaryStage.show();

        if (!isPlayer1Human && !isPlayer2Human) {
            startAITurns(primaryStage, xIcon, oIcon);
        } else if (!isPlayer1Human) {
            PauseTransition pause = new PauseTransition(Duration.seconds(1));
            pause.setOnFinished(event -> playAIMove(primaryStage, xIcon, oIcon, player1Depth, true));
            pause.play();
        }
    }

    /**
     * Creates a power-up button for a player.
     *
     * @param powerup    The power-up type.
     * @param playerLabel The label to display on the button.
     * @param isPlayer1  Indicates if the power-up is for Player 1.
     * @param buttonBox  The container for the button.
     */
    private void createPowerupButton(String powerup, String playerLabel, boolean isPlayer1, HBox buttonBox) {
        Button powerupButton = new Button(playerLabel + ": " + powerup);
        powerupButton.setFont(new Font("Arial", 16));

        if (isPlayer1) {
            powerupButton.setStyle(
                    "-fx-background-color: black;" +
                            "-fx-text-fill: linear-gradient(cyan, #00FFFF);" +
                            "-fx-border-color: cyan;" +
                            "-fx-border-width: 3;" +
                            "-fx-effect: dropshadow(gaussian, #00FFFF, 10, 0.5, 0, 0);"
            );
        } else {
            powerupButton.setStyle(
                    "-fx-background-color: black;" +
                            "-fx-text-fill: linear-gradient(magenta, #FF00FF);" +
                            "-fx-border-color: magenta;" +
                            "-fx-border-width: 3;" +
                            "-fx-effect: dropshadow(gaussian, #FF00FF, 10, 0.5, 0, 0);"
            );
        }

        switch (powerup) {
            case "Tile Swapper" -> powerupButton.setOnAction(e -> {
                if (isPlayer1Turn == isPlayer1 && isPowerupUsed(isPlayer1)) {
                    activateTileSwapper();
                    markPowerupUsed(isPlayer1);
                    powerupButton.setDisable(true);
                }
            });
            case "Tile Reserver" -> powerupButton.setOnAction(e -> {
                if (isPlayer1Turn == isPlayer1 && isPowerupUsed(isPlayer1)) {
                    activateTileReserver(isPlayer1);
                    markPowerupUsed(isPlayer1);
                    powerupButton.setDisable(true);
                }
            });
            case "Tile Remover" -> powerupButton.setOnAction(e -> {
                if (isPlayer1Turn == isPlayer1 && isPowerupUsed(isPlayer1)) {
                    activateTileRemover(isPlayer1);
                    markPowerupUsed(isPlayer1);
                    powerupButton.setDisable(true);
                }
            });
            case "Inverter" -> powerupButton.setOnAction(e -> {
                if (isPlayer1Turn == isPlayer1 && isPowerupUsed(isPlayer1)) {
                    activateInverter();
                    markPowerupUsed(isPlayer1);
                    powerupButton.setDisable(true);
                }
            });
        }

        if (isPlayer1) {
            buttonBox.getChildren().add(0, powerupButton);
        } else {
            buttonBox.getChildren().add(powerupButton);
        }

        if (isPlayer1) {
            player1PowerupButton = powerupButton;
        } else {
            player2PowerupButton = powerupButton;
        }
    }


    /**
     * Checks if a power-up has been used for a given player.
     *
     * @param isPlayer1 Indicates if it's Player 1.
     * @return True if the power-up has been used, false otherwise.
     */
    private boolean isPowerupUsed(boolean isPlayer1) {
        return isPlayer1 ? !player1PowerupUsed : !player2PowerupUsed;
    }

    /**
     * Marks the power-up as used for a given player.
     *
     * @param isPlayer1 Indicates if it's Player 1.
     */
    private void markPowerupUsed(boolean isPlayer1) {
        if (isPlayer1) {
            player1PowerupUsed = true;
        } else {
            player2PowerupUsed = true;
        }
    }
    /**
     * Activates the Inverter power-up, allowing the player to select a tile
     * and invert the row, column, or diagonal.
     */
    private void activateInverter() {
        System.out.println("Inverter activated. Select a tile to invert.");

        grid.setStyle("-fx-background-color: radial-gradient(radius 100%, black, #111);");

        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                button.setOnAction(e -> handleInverterSelection(button));
            }
        });
    }

    /**
     * Handles the selection of a tile for the Inverter power-up.
     *
     * @param selectedButton The selected button for inversion.
     */
    private void handleInverterSelection(Button selectedButton) {
        int row = GridPane.getRowIndex(selectedButton);
        int col = GridPane.getColumnIndex(selectedButton);

        highlightRowColumnDiagonal(row, col);

        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                button.setOnAction(e -> handleInverterRowColumnDiagonalSelection(row, col, button));
            }
        });
    }

    /**
     * Highlights the row, column, and diagonal of the selected tile for inversion.
     *
     * @param row The row of the selected tile.
     * @param col The column of the selected tile.
     */
    private void highlightRowColumnDiagonal(int row, int col) {
        for (int i = 0; i < 5; i++) {
            Button rowButton = getNodeFromGridPane(grid, i, row);
            assert rowButton != null;
            rowButton.setStyle("-fx-background-color: black; -fx-border-color: yellow; -fx-border-width: 3px; -fx-effect: dropshadow(gaussian, yellow, 15, 0.5, 0, 0);");
        }

        for (int i = 0; i < 5; i++) {
            Button colButton = getNodeFromGridPane(grid, col, i);
            assert colButton != null;
            colButton.setStyle("-fx-background-color: black; -fx-border-color: yellow; -fx-border-width: 3px; -fx-effect: dropshadow(gaussian, yellow, 15, 0.5, 0, 0);");
        }

        if (row == col || row + col == 4) {
            for (int i = 0; i < 5; i++) {
                Button diagButton = getNodeFromGridPane(grid, i, i);
                assert diagButton != null;
                diagButton.setStyle("-fx-background-color: black; -fx-border-color: yellow; -fx-border-width: 3px; -fx-effect: dropshadow(gaussian, yellow, 15, 0.5, 0, 0);");
                if (row + col == 4) {
                    Button antiDiagButton = getNodeFromGridPane(grid, i, 4 - i);
                    assert antiDiagButton != null;
                    antiDiagButton.setStyle("-fx-background-color: black; -fx-border-color: yellow; -fx-border-width: 3px; -fx-effect: dropshadow(gaussian, yellow, 15, 0.5, 0, 0);");
                }
            }
        }
    }

    /**
     * Handles the selection of which part to invert (row, column, or diagonal).
     *
     * @param selectedRow The selected row.
     * @param selectedCol The selected column.
     * @param secondButton The second selected button to perform the inversion.
     */
    private void handleInverterRowColumnDiagonalSelection(int selectedRow, int selectedCol, Button secondButton) {
        int secondRow = GridPane.getRowIndex(secondButton);
        int secondCol = GridPane.getColumnIndex(secondButton);

        if (secondRow == selectedRow) {
            System.out.println("Inverting row " + selectedRow);
            invertRow(selectedRow);
        } else if (secondCol == selectedCol) {
            System.out.println("Inverting column " + secondCol);
            invertColumn(secondCol);
        } else if ((selectedRow == selectedCol && secondRow == secondCol) || (selectedRow + selectedCol == 4 && secondRow + secondCol == 4)) {
            if (selectedRow == selectedCol) {
                System.out.println("Inverting main diagonal");
                invertDiagonal(true);
            } else {
                System.out.println("Inverting anti-diagonal");
                invertDiagonal(false);
            }
        }

        checkForWinOrDraw();

        if (!gameOver) {
            grid.setStyle("-fx-background-color: black;");
            resetTileBorders();

            resetCellClickHandlers();

            togglePlayerTurn();

            if (!isPlayer1Turn && !isPlayer2Human) {
                PauseTransition pause = new PauseTransition(Duration.seconds(1));
                pause.setOnFinished(e -> playAIMove(primaryStage, xIcon, oIcon, player2Depth, false));
                pause.play();
            } else if (isPlayer1Turn && !isPlayer1Human) {
                PauseTransition pause = new PauseTransition(Duration.seconds(1));
                pause.setOnFinished(e -> playAIMove(primaryStage, xIcon, oIcon, player1Depth, true));
                pause.play();
            }
        }
    }
    /**
     * Checks for win or draw conditions after an inversion or normal gameplay.
     */
    private void checkForWinOrDraw() {
        boolean player1Wins = board.checkWin('x');
        boolean player2Wins = board.checkWin('o');

        if (player1PowerupUsed ^ player2PowerupUsed) {
            if (player1Wins && player2Wins) {
                gameOver = true;
                displayEndGameMessage("Draw!", primaryStage);
            }
        }
        else if (player1Wins) {
            gameOver = true;
            displayEndGameMessage("Player 1 Wins!", primaryStage);
        }
        else if (player2Wins) {
            gameOver = true;
            displayEndGameMessage("Player 2 Wins!", primaryStage);
        }
        else if (board.isFull()) {
            gameOver = true;
            displayEndGameMessage("Draw!", primaryStage);
        }
    }
    /**
     * Resets the cell click handlers to restore default gameplay.
     */
    private void resetCellClickHandlers() {
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                button.setOnAction(e -> handleCellClick(button, xIcon, oIcon, primaryStage));
            }
        });
    }

    /**
     * Inverts a row on the board.
     *
     * @param row The row to invert.
     */
    private void invertRow(int row) {
        List<Button> rowButtons = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            rowButtons.add(getNodeFromGridPane(grid, i, row));
        }
        invertButtonList(rowButtons);
    }

    /**
     * Inverts a column on the board.
     *
     * @param col The column to invert.
     */
    private void invertColumn(int col) {
        List<Button> colButtons = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            colButtons.add(getNodeFromGridPane(grid, col, i));
        }
        invertButtonList(colButtons);
    }

    /**
     * Inverts a diagonal on the board.
     *
     * @param isMainDiagonal True if it's the main diagonal, false if it's the anti-diagonal.
     */
    private void invertDiagonal(boolean isMainDiagonal) {
        List<Button> diagonalButtons = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            if (isMainDiagonal) {
                diagonalButtons.add(getNodeFromGridPane(grid, i, i));
            } else {
                diagonalButtons.add(getNodeFromGridPane(grid, i, 4 - i));
            }
        }
        invertButtonList(diagonalButtons);
    }

    /**
     * Inverts the buttons in a list by swapping their symbols.
     *
     * @param buttons The list of buttons to invert.
     */
    private void invertButtonList(List<Button> buttons) {
        List<ImageView> buttonGraphics = new ArrayList<>();
        for (Button button : buttons) {
            if (button.getGraphic() instanceof ImageView) {
                buttonGraphics.add((ImageView) button.getGraphic());
            } else {
                buttonGraphics.add(null);
            }
        }
        Collections.reverse(buttonGraphics);
        for (int i = 0; i < buttons.size(); i++) {
            buttons.get(i).setGraphic(buttonGraphics.get(i));
        }
        for (int i = 0; i < buttons.size(); i++) {
            int row = GridPane.getRowIndex(buttons.get(i));
            int col = GridPane.getColumnIndex(buttons.get(i));
            char symbol;
            if (buttonGraphics.get(i) == null) {
                symbol = ' ';
            } else if (buttonGraphics.get(i).getImage().getUrl().contains("fraudx2.png")) {
                symbol = 'x';
            } else if (buttonGraphics.get(i).getImage().getUrl().contains("circle.png")) {
                symbol = 'o';
            } else {
                symbol = ' ';
            }
            board.setSymbolAt(row, col, symbol);
        }
    }

    /**
     * Resets the tile borders to the default neon theme.
     */
    private void resetTileBorders() {
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                setButtonBorder(button, "cyan");
            }
        });
    }


    /**
     * Gets the node from the GridPane at the specified row and column.
     *
     * @param gridPane The grid pane.
     * @param col      The column index.
     * @param row      The row index.
     * @return The button at the specified row and column.
     */
    private Button getNodeFromGridPane(GridPane gridPane, int col, int row) {
        for (javafx.scene.Node node : gridPane.getChildren()) {
            if (GridPane.getColumnIndex(node) == col && GridPane.getRowIndex(node) == row) {
                return (Button) node;
            }
        }
        return null;
    }

    /**
     * Activates the Tile Remover power-up, removing two opponent tiles randomly.
     *
     * @param isPlayer1 True if the current player is Player 1.
     */
    private void activateTileRemover(boolean isPlayer1) {
        char opponentSymbol = isPlayer1 ? 'o' : 'x';
        List<Button> opponentCells = new ArrayList<>();
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                if (getSymbolFromButton(button) == opponentSymbol) {
                    opponentCells.add(button);
                }
            }
        });
        if (opponentCells.size() >= 2) {
            Collections.shuffle(opponentCells);
            Button cell1 = opponentCells.get(0);
            Button cell2 = opponentCells.get(1);
            eraseCell(cell1);
            eraseCell(cell2);
        }
    }


    /**
     * Erases a cell, resetting it for future moves.
     *
     * @param cellButton The button representing the cell to erase.
     */
    private void eraseCell(Button cellButton) {
        cellButton.setGraphic(null);
        setButtonBorder(cellButton, "cyan");
        int row = GridPane.getRowIndex(cellButton);
        int col = GridPane.getColumnIndex(cellButton);
        board.setSymbolAt(row, col, ' ');
        cellButton.setOnAction(e -> handleCellClick(cellButton, xIcon, oIcon, primaryStage));
    }

    /**
     * Extracts the symbol from a button (X, O, or empty).
     *
     * @param cellButton The button to extract the symbol from.
     * @return The symbol represented in the button.
     */
    private char getSymbolFromButton(Button cellButton) {
        if (cellButton.getGraphic() != null) {
            ImageView imageView = (ImageView) cellButton.getGraphic();
            Image image = imageView.getImage();
            if (image.getUrl().contains("fraudx2.png")) {
                return 'x';
            } else if (image.getUrl().contains("circle.png")) {
                return 'o';
            }
        }
        return ' ';
    }

    /**
     * Activates the Tile Reserver power-up for the player, allowing them to reserve a tile.
     *
     * @param isPlayer1 True if Player 1 is using the power-up.
     */
    private void activateTileReserver(boolean isPlayer1) {
        resetTileReserverState();
        tileReserverActive = true;

        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                if (button.getGraphic() == null) {
                    button.setStyle("-fx-background-color: black; -fx-border-color: blue; -fx-border-width: 3;");
                    button.setOnAction(e -> reserveTile(button, isPlayer1));
                }
            }
        });
    }


    /**
     * Resets all tile reserver states for both players and resets click handlers.
     */
    private void resetTileReserverState() {
        tileReserverActive = false;

        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                button.setStyle("-fx-background-color: black; -fx-border-color: cyan; -fx-border-width: 2;");
                button.setOnAction(e -> handleCellClick(button, xIcon, oIcon, primaryStage));
            }
        });

        reservedTileCountdown.clear();
    }

    /**
     * Reserves the selected tile for the player when they activate the Tile Reserver power-up.
     *
     * @param clickedTile The tile to reserve.
     * @param isPlayer1   True if Player 1 is reserving the tile.
     */
    private void reserveTile(Button clickedTile, boolean isPlayer1) {
        if (!tileReserverActive) return;

        if (isPlayer1 && reservedTilePlayer1 == null && clickedTile.getGraphic() == null) {
            reservedTilePlayer1 = clickedTile;
            reservedTileCountdownPlayer1.put(clickedTile, 4);
            forceRedBorder(clickedTile);
        } else if (!isPlayer1 && reservedTilePlayer2 == null && clickedTile.getGraphic() == null) {
            reservedTilePlayer2 = clickedTile;
            reservedTileCountdownPlayer2.put(clickedTile, 4);
            forceRedBorder(clickedTile);
        }

        tileReserverActive = false;

        deactivateTileReserver();
        togglePlayerTurn();
    }

    /**
     * Changes the border of a tile to red when it is reserved.
     *
     * @param tile The tile to mark with a red border.
     */
    private void forceRedBorder(Button tile) {
        Platform.runLater(() -> {
            tile.setStyle("-fx-background-color: black; -fx-border-color: red; -fx-border-width: 3;");
            tile.applyCss();
            tile.layout();
        });
    }


    /**
     * Deactivates the Tile Reserver power-up and resets click handlers for normal gameplay.
     */
    private void deactivateTileReserver() {
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                if (button != reservedTilePlayer1 && button != reservedTilePlayer2) {
                    button.setStyle("-fx-background-color: black; -fx-border-color: cyan; -fx-border-width: 2;");
                    button.setOnAction(e -> handleCellClick(button, xIcon, oIcon, primaryStage));
                }
            }
        });
    }

    /**
     * Updates the countdown for the reserved tiles and resets them when the countdown expires.
     */
    private void updateReservedTileCountdown() {
        new HashMap<>(reservedTileCountdownPlayer1).forEach((button, countdown) -> {
            if (countdown > 0) {
                reservedTileCountdownPlayer1.put(button, countdown - 1);
                forceRedBorder(button);
                if (countdown == 1) {
                    resetTileToCyan(button);
                    reservedTileCountdownPlayer1.remove(button);
                    reservedTilePlayer1 = null;
                }
            }
        });

        new HashMap<>(reservedTileCountdownPlayer2).forEach((button, countdown) -> {
            if (countdown > 0) {
                reservedTileCountdownPlayer2.put(button, countdown - 1);
                forceRedBorder(button);
                if (countdown == 1) {
                    resetTileToCyan(button);
                    reservedTileCountdownPlayer2.remove(button);
                    reservedTilePlayer2 = null;
                }
            }
        });
    }

    /**
     * Resets a tile to its default cyan color and re-enables it for future gameplay after expiration.
     *
     * @param tile The tile to reset.
     */
    private void resetTileToCyan(Button tile) {
        Platform.runLater(() -> {
            tile.setStyle("-fx-background-color: black; -fx-border-color: cyan; -fx-border-width: 2;");
            tile.applyCss();
            tile.layout();
            tile.setOnAction(e -> handleCellClick(tile, xIcon, oIcon, primaryStage));
        });
    }

    /**
     * Activates the Tile Swapper power-up, allowing players to swap two tiles.
     */
    private void activateTileSwapper() {
        tileSwapperActive = true;
        storeGridColors();
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                button.setStyle("-fx-background-color: black; -fx-border-color: red; -fx-border-width: 3;");
                button.setOnAction(e -> handleTileSelection(button));
            }
        });
    }

    /**
     * Handles the selection of tiles for swapping when Tile Swapper is active.
     *
     * @param clickedTile The selected tile.
     */
    private void handleTileSelection(Button clickedTile) {
        if (!tileSwapperActive) return;
        if (firstSelectedTile == null) {
            firstSelectedTile = clickedTile;
            firstSelectedTile.setStyle("-fx-background-color: black; -fx-border-color: yellow; -fx-border-width: 3;");
        }
        else if (secondSelectedTile == null && clickedTile != firstSelectedTile) {
            secondSelectedTile = clickedTile;
            secondSelectedTile.setStyle("-fx-background-color: black; -fx-border-color: yellow; -fx-border-width: 3;");
            swapTiles(firstSelectedTile, secondSelectedTile);
            checkGameAfterSwap();
            deactivateTileSwapper();
        }
    }

    /**
     * Swaps the symbols between two selected tiles.
     *
     * @param tile1 The first selected tile.
     * @param tile2 The second selected tile.
     */
    private void swapTiles(Button tile1, Button tile2) {
        int row1 = GridPane.getRowIndex(tile1);
        int col1 = GridPane.getColumnIndex(tile1);
        int row2 = GridPane.getRowIndex(tile2);
        int col2 = GridPane.getColumnIndex(tile2);

        char symbol1 = board.getSymbolAt(row1, col1);
        char symbol2 = board.getSymbolAt(row2, col2);

        board.setSymbolAt(row1, col1, symbol2);
        board.setSymbolAt(row2, col2, symbol1);

        Node tempGraphic = tile1.getGraphic();
        tile1.setGraphic(tile2.getGraphic());
        tile2.setGraphic(tempGraphic);
    }

    /**
     * Checks the game state after a swap to determine if there is a win or draw.
     */
    private void checkGameAfterSwap() {
        boolean player1Wins = board.checkWin('x');
        boolean player2Wins = board.checkWin('o');

        if (player1Wins && player2Wins) {
            displayEndGameMessage("Draw!", primaryStage);
            gameOver = true;
        } else if (player1Wins) {
            displayEndGameMessage("Player 1 Wins!", primaryStage);
            gameOver = true;
        } else if (player2Wins) {
            displayEndGameMessage("Player 2 Wins!", primaryStage);
            gameOver = true;
        } else if (board.isFull()) {
            displayEndGameMessage("Draw!", primaryStage);
            gameOver = true;
        } else {
            togglePlayerTurn();
            handleAIMove();
        }
    }

    /**
     * Deactivates the Tile Swapper power-up and resets the grid.
     */
    private void deactivateTileSwapper() {
        tileSwapperActive = false;
        firstSelectedTile = null;
        secondSelectedTile = null;
        restoreGridColors();
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                button.setOnAction(e -> handleCellClick(button, xIcon, oIcon, primaryStage));
            }
        });
    }

    /**
     * Stores the original border colors of the grid for later restoration.
     */
    private void storeGridColors() {
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                originalBorderColors.put(button, button.getStyle());
            }
        });
    }

    /**
     * Restores the original border colors of the grid after power-up deactivation.
     */
    private void restoreGridColors() {
        grid.getChildren().forEach(node -> {
            if (node instanceof Button button) {
                if (originalBorderColors.containsKey(button)) {
                    button.setStyle(originalBorderColors.get(button));
                }
            }
        });
    }

    /**
     * Switches the player's turn and updates the board colors to reflect the current turn.
     */
    private void togglePlayerTurn() {
        isPlayer1Turn = !isPlayer1Turn;
        updateBoardColors();
        handleAIMove();
    }


    /**
     * Handles the AI move depending on whose turn it is.
     */
    private void handleAIMove() {
        if (!isPlayer1Turn && !isPlayer2Human) {
            PauseTransition pause = new PauseTransition(Duration.seconds(1));
            pause.setOnFinished(e -> playAIMove(primaryStage, xIcon, oIcon, player2Depth, false));
            pause.play();
        } else if (isPlayer1Turn && !isPlayer1Human) {
            PauseTransition pause = new PauseTransition(Duration.seconds(1));
            pause.setOnFinished(e -> playAIMove(primaryStage, xIcon, oIcon, player1Depth, true));
            pause.play();
        }
    }


    /**
     * Handles the logic for when a cell is clicked. If the game is ongoing, and it's the human player's turn,
     * it makes a move. If the next player is an AI, it schedules the AI's move with a delay.
     *
     * @param cellButton   The button representing the clicked cell.
     * @param xIcon        The image representing Player 1 (X).
     * @param oIcon        The image representing Player 2 (O).
     * @param primaryStage The primary stage of the application.
     */
    private void handleCellClick(Button cellButton, Image xIcon, Image oIcon, Stage primaryStage) {
        if (gameOver || (!isPlayer1Human && isPlayer1Turn) || (!isPlayer2Human && !isPlayer1Turn)) {
            return;
        }

        makeMove(cellButton, xIcon, oIcon, primaryStage);

        if (!gameOver) {
            if (!isPlayer1Turn && !isPlayer2Human) {
                PauseTransition pause = new PauseTransition(Duration.seconds(0.75));
                pause.setOnFinished(e -> playAIMove(primaryStage, xIcon, oIcon, player2Depth, false));
                pause.play();
            } else if (isPlayer1Turn && !isPlayer1Human) {
                PauseTransition pause = new PauseTransition(Duration.seconds(0.75));
                pause.setOnFinished(e -> playAIMove(primaryStage, xIcon, oIcon, player1Depth, true));
                pause.play();
            }
        }
    }

    /**
     * Starts the AI gameplay loop if both players are AI. The game alternates between Player 1 and Player 2,
     * continuously triggering AI moves with a fixed delay between turns.
     *
     * @param primaryStage The primary stage of the application.
     * @param xIcon        The image representing Player 1 (X).
     * @param oIcon        The image representing Player 2 (O).
     */
    private void startAITurns(Stage primaryStage, Image xIcon, Image oIcon) {
        Timeline aiTimeline = new Timeline(new KeyFrame(Duration.seconds(1), event -> {
            if (!gameOver) {
                if (isPlayer1Turn) {
                    playAIMove(primaryStage, xIcon, oIcon, player1Depth, true);
                } else {
                    playAIMove(primaryStage, xIcon, oIcon, player2Depth, false);
                }
            }
        }));
        aiTimeline.setCycleCount(Timeline.INDEFINITE);
        aiTimeline.play();
    }

    /**
     * Plays the AI move using the minimax algorithm to determine the best move.
     *
     * @param primaryStage The primary stage.
     * @param xIcon        The image representing Player 1 (X).
     * @param oIcon        The image representing Player 2 (O).
     * @param depth        The depth level for the minimax AI algorithm.
     * @param isPlayer1    True if Player 1 is the AI making the move.
     */
    private void playAIMove(Stage primaryStage, Image xIcon, Image oIcon, int depth, boolean isPlayer1) {
        if (gameOver) return;

        int[] bestMove = MiniMax.findBestMove(board, depth, isPlayer1);

        if (bestMove != null) {
            int row = bestMove[0];
            int col = bestMove[1];
            Button cellButton = getNodeFromGridPane(grid, col, row);
            makeMove(cellButton, xIcon, oIcon, primaryStage);
        }
    }

    /**
     * Handles making a move on the selected cell.
     *
     * @param cellButton   The button representing the selected cell.
     * @param xIcon        The image representing Player 1 (X).
     * @param oIcon        The image representing Player 2 (O).
     * @param primaryStage The primary stage.
     */
    private void makeMove(Button cellButton, Image xIcon, Image oIcon, Stage primaryStage) {
        if (gameOver || cellButton.getGraphic() != null) {
            showInvalidMoveMessage(cellButton);
            return;
        }

        if (reservedTileCountdown.getOrDefault(cellButton, 0) > 0) {
            if ((isPlayer1Turn && reservedTileCountdown.get(cellButton) % 2 == 0) ||
                    (!isPlayer1Turn && reservedTileCountdown.get(cellButton) % 2 != 0)) {
                showInvalidMoveMessage(cellButton);
                return;
            }
        }

        ImageView imageView = new ImageView(isPlayer1Turn ? xIcon : oIcon);
        imageView.setPreserveRatio(true);
        imageView.setFitWidth(90);
        imageView.setFitHeight(90);
        cellButton.setGraphic(imageView);

        int row = GridPane.getRowIndex(cellButton);
        int col = GridPane.getColumnIndex(cellButton);

        char currentPlayerSymbol = isPlayer1Turn ? 'x' : 'o';
        board.makeMove(row, col, currentPlayerSymbol);

        reservedTileCountdown.remove(cellButton);

        if (board.checkWin(currentPlayerSymbol)) {
            gameOver = true;
            displayEndGameMessage(isPlayer1Turn ? "Player 1 Wins" : "Player 2 Wins", primaryStage);
        } else if (board.isFull()) {
            gameOver = true;
            displayEndGameMessage("Draw", primaryStage);
        } else {
            isPlayer1Turn = !isPlayer1Turn;
            updateBoardColors();
            updateReservedTileCountdown();
        }
    }


    /**
     * Displays a message when the player attempts an invalid move.
     *
     * @param cellButton The button that was clicked.
     */
    private void showInvalidMoveMessage(Button cellButton) {
        Node originalGraphic = cellButton.getGraphic();
        Label invalidMoveLabel = new Label("Invalid");
        invalidMoveLabel.setFont(new Font("Arial", 20));
        invalidMoveLabel.setStyle(
                "-fx-text-fill: linear-gradient(red, #FF0000);" +
                        "-fx-effect: dropshadow(gaussian, #FF0000, 10, 0, 0, 0);"
        );
        cellButton.setGraphic(invalidMoveLabel);

        Timeline timeline = new Timeline(new KeyFrame(Duration.seconds(1), e -> cellButton.setGraphic(originalGraphic)));
        timeline.play();
    }

    /**
     * Updates the colors of the board based on the current player's turn.
     */
    public void updateBoardColors() {
        String currentPlayerColor = isPlayer1Turn ? PLAYER_1_COLOR : PLAYER_2_COLOR;
        grid.getChildren().forEach(node -> {
            if (node instanceof Button btn) {
                if (btn.getGraphic() == null) {
                    setButtonBorder(btn, currentPlayerColor);
                }
            }
        });
    }

    /**
     * Sets the border color for a button to a specified color.
     *
     * @param button The button to update.
     * @param color  The color for the border.
     */
    public void setButtonBorder(Button button, String color) {
        button.setStyle(
                "-fx-background-color: transparent; " +
                        "-fx-border-color: " + color + "; " +
                        "-fx-border-width: 2px;"
        );
    }

    /**
     * Displays the end game message when the game is over.
     *
     * @param message      The message to display.
     * @param primaryStage The primary stage.
     */
    public void displayEndGameMessage(String message, Stage primaryStage) {
        endGameLabel = new Label(message);
        endGameLabel.setFont(new Font("Impact", 50));
        endGameLabel.setStyle(
                "-fx-text-fill: linear-gradient(red, #FF0000);" +
                        "-fx-effect: dropshadow(gaussian, #FF0000, 20, 0, 0, 0);"
        );
        StackPane.setAlignment(endGameLabel, Pos.CENTER);
        StackPane root = (StackPane) primaryStage.getScene().getRoot();
        root.getChildren().add(endGameLabel);
    }

    /**
     * Resets the game, clearing the board and resetting all states.
     *
     * @param xIcon        The image for Player 1 (X).
     * @param oIcon        The image for Player 2 (O).
     * @param primaryStage The primary stage.
     */
    private void resetGame(Image xIcon, Image oIcon, Stage primaryStage) {
        board = new TicTacToe.Board();
        isPlayer1Turn = true;
        gameOver = false;

        for (int row = 0; row < 5; row++) {
            for (int col = 0; col < 5; col++) {
                Button button = getNodeFromGridPane(grid, col, row);
                assert button != null;
                button.setGraphic(null);
                setButtonBorder(button, PLAYER_1_COLOR);

                button.setOnAction(e -> handleCellClick(button, xIcon, oIcon, primaryStage));
            }
        }

        player1PowerupUsed = false;
        player2PowerupUsed = false;
        reservedTileCountdown.clear();

        if (player1Powerup != null) {
            if (player1PowerupButton == null) {
                createPowerupButton(player1Powerup, "Player 1", true, buttonBox);
            } else {
                player1PowerupButton.setDisable(false);
            }
        }

        if (player2Powerup != null) {
            if (player2PowerupButton == null) {
                createPowerupButton(player2Powerup, "Player 2", false, buttonBox);
            } else {
                player2PowerupButton.setDisable(false);
            }
        }

        if (endGameLabel != null) {
            StackPane root = (StackPane) primaryStage.getScene().getRoot();
            root.getChildren().remove(endGameLabel);
            endGameLabel = null;
        }

        resetPowerUpState();

        if (!isPlayer1Human) {
            PauseTransition pause = new PauseTransition(Duration.seconds(1));
            pause.setOnFinished(event -> playAIMove(primaryStage, xIcon, oIcon, player1Depth, true));
            pause.play();
        }
    }

    /**
     * Resets the state of all power-ups after the game is reset.
     */
    private void resetPowerUpState() {
        tileReserverActive = false;
        reservedTileCountdown.clear();
    }
    public static void main(String[] args) {
        launch(args);
    }
}
