package com.example.GUIA3.logic;

public class TicTacToe {

    private static final int size = 5;
    private static final char empty = '.';
    private static final char x = 'x';
    private static final char o = 'o';

    public static class Board {
        char [][] grid;

        public Board() {
            grid = new char[size][size];
            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    grid[i][j] = empty;
                }
            }
        }
        // Getter to access the grid value
        public char getSymbolAt(int row, int col) {
            return grid[row][col];
        }

        // Setter to modify the grid value
        public void setSymbolAt(int row, int col, char symbol) {
            grid[row][col] = symbol;
        }

        public boolean isFull() {
            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    if (grid[i][j] == empty) {
                        return false;
                    }
                }
            }
            return true;
        }

        public void makeMove(int row, int col, char player) {
            grid[row][col] = player;
        }

        // New method to swap two tiles on the board
        public boolean swapTiles(int row1, int col1, int row2, int col2) {
            // Swap the two tiles in the backend
            char temp = grid[row1][col1];
            grid[row1][col1] = grid[row2][col2];
            grid[row2][col2] = temp;

            return true;
        }

        // Check if the position is valid on the board
        private boolean isValidPosition(int row, int col) {
            return row >= 0 && row < size && col >= 0 && col < size;
        }

        public boolean checkRow(int row, char player) {
            for (int col = 0; col < size; col++) {
                if (grid[row][col] != player) {
                    return false;
                }
            }
            return true;
        }

        public boolean checkCol(int col, char player) {
            for (int row = 0; row < size; row++) {
                if (grid[row][col] != player) {
                    return false;
                }
            }
            return true;
        }

        public boolean checkDiagonal(char player) {
            boolean leftDiagonal = true;
            boolean rightDiagonal = true;

            for (int i = 0; i < size; i++) {
                if (grid[i][i] != player) {
                    leftDiagonal = false;
                }
                if (grid[i][size - i - 1] != player) {
                    rightDiagonal = false;
                }
            }

            return leftDiagonal || rightDiagonal;
        }

        public boolean checkWin(char player) {
            for (int i = 0; i < size; i++) {
                if (checkRow(i, player) || checkCol(i, player)) {
                    return true;
                }
            }
            return checkDiagonal(player);
        }

        public double evaluate() {
            if (checkWin(x)) {
                return 1.0;
            }
            else if (checkWin(o)) {
                return -1.0;
            }
            double scoreX = evaluatePlayer(x);
            double scoreO = evaluatePlayer(o);

            return (scoreX - scoreO) / (scoreX + scoreO);
        }

        public double evaluatePlayer(char player) {
            double score = 0;

            for (int i = 0; i < size; i++) {
                score += evaluateLine(getRow(i), player);
                score += evaluateLine(getColumn(i), player);
            }
            score += evaluateLine(getLeftDiagonal(), player);
            score += evaluateLine(getRightDiagonal(), player);

            return score;
        }

        public double evaluateLine(char[] line, char player) {
            int consecutive = 0;
            int blocks = 0;
            double score;

            for (char cell : line) {
                if (cell == player) {
                    consecutive++;
                } else if (cell != empty) {
                    blocks++;
                }
            }

            // Scoring based on consecutive marks
            if (consecutive == size) {
                return 1.0;
            } else if (consecutive == 0) {
                return 0;
            } else {
                // Partial progress -> more consecutive marks and fewer blocks = higher score
                score = (double) consecutive / size;
                if (blocks == 0) {
                    score *= 2; // No blocks = double the score
                } else {
                    score /= blocks; // Reduce score proportional to amount of blocks
                }
            }
            return score;
        }

        public char[] getRow(int row) {
            return grid[row];
        }

        public char[] getColumn(int col) {
            char[] column = new char[size];
            for (int i = 0; i < size; i++) {
                column[i] = grid[i][col];
            }
            return column;
        }

        public char[] getLeftDiagonal() {
            char[] diagonal = new char[size];
            for (int i = 0; i < size; i++) {
                diagonal[i] = grid[i][i];
            }
            return diagonal;
        }

        public char[] getRightDiagonal() {
            char[] diagonal = new char[size];
            for (int i = 0; i < size; i++) {
                diagonal[i] = grid[i][size - i - 1];
            }
            return diagonal;
        }
    }
}
