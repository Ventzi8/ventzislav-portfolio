package com.example.GUIA3.logic;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * This class implements the minimax algorithm with alpha-beta pruning to determine
 * the best move in a Tic-Tac-Toe game. It recursively evaluates potential moves for
 * both the maximizing and minimizing players, choosing optimal moves by comparing
 * heuristic board values and pruning unnecessary branches to optimize the search.
 *
 * @author 643270vi Ventzi Ivanov
 */
public class MiniMax {

    private static final int size = 5;
    static Random random = new Random();
    static final char x = 'x';
    static final char o = 'o';
    static final char empty = '.';

    /**
     * Implements the minimax algorithm with alpha-beta pruning.
     *
     * @param board - Current state of the game.
     * @param depth - Maximum depth for the recursive search.
     * @param alpha - Best value that the maximizing player can guarantee.
     * @param beta - Best value that the minimizing player can guarantee.
     * @param maximizingPlayer - True if the current move is for Player X (maximizing player).
     * @return The heuristic value of the board.
     */
    public static double minimax(TicTacToe.Board board, int depth, double alpha, double beta, boolean maximizingPlayer) {
        if (depth < 0) {
            return 0;
        }

        if (depth == 0 || board.checkWin(x) || board.checkWin(o) || board.isFull()) {
            return board.evaluate();
        }

        if (maximizingPlayer) {
            double maxEval = Double.NEGATIVE_INFINITY;
            for (int[] move : getAvailableMoves(board)) {
                board.makeMove(move[0], move[1], x);
                double eval = minimax(board, depth - 1, alpha, beta, false);
                board.makeMove(move[0], move[1], empty);
                maxEval = Math.max(maxEval, eval);
                alpha = Math.max(alpha, eval);
                if (beta <= alpha) {
                    break;
                }
            }
            return maxEval;
        } else {
            double minEval = Double.POSITIVE_INFINITY;
            for (int[] move : getAvailableMoves(board)) {
                board.makeMove(move[0], move[1], o);
                double eval = minimax(board, depth - 1, alpha, beta, true);
                board.makeMove(move[0], move[1], empty);
                minEval = Math.min(minEval, eval);
                beta = Math.min(beta, eval);
                if (beta <= alpha) {
                    break;
                }
            }
            return minEval;
        }
    }

    /**
     * Finds the best move for the current player, given a board and a depth.
     * If depth = 0, randomly selects a valid move.
     *
     * @param board - Current state of the game.
     * @param depth - Maximum depth for the recursive search.
     * @param maximizingPlayer - True if it's the maximizing player's turn (Player X).
     * @return The best move as an array [row, col].
     */
    public static int[] findBestMove(TicTacToe.Board board, int depth, boolean maximizingPlayer) {
        List<int[]> availableMoves = getAvailableMoves(board);

        if (availableMoves.isEmpty()) {
            return null;
        }

        double alpha = Double.NEGATIVE_INFINITY;
        double beta = Double.POSITIVE_INFINITY;

        List<int[]> bestMoves = new ArrayList<>();
        double bestValue = maximizingPlayer ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;

        for (int[] move : availableMoves) {
            board.makeMove(move[0], move[1], maximizingPlayer ? x : o);
            double moveValue = minimax(board, depth - 1, alpha, beta, !maximizingPlayer);
            board.makeMove(move[0], move[1], empty);

            if (maximizingPlayer) {
                if (moveValue > bestValue) {
                    bestValue = moveValue;
                    bestMoves.clear();
                    bestMoves.add(move);
                } else if (moveValue == bestValue) {
                    bestMoves.add(move);
                }
                alpha = Math.max(alpha, moveValue);
            } else {
                if (moveValue < bestValue) {
                    bestValue = moveValue;
                    bestMoves.clear();
                    bestMoves.add(move);
                } else if (moveValue == bestValue) {
                    bestMoves.add(move);
                }
                beta = Math.min(beta, moveValue);
            }

            if (beta <= alpha) {
                break;
            }
        }

        return bestMoves.isEmpty() ? null : bestMoves.get(random.nextInt(bestMoves.size()));
    }

    /**
     * Get the list of all available moves (empty spaces) on the board.
     *
     * @param board - Current state of the game.
     * @return A list of available moves as arrays [row, col].
     */
    public static List<int[]> getAvailableMoves(TicTacToe.Board board) {
        List<int[]> availableMoves = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                if (board.grid[i][j] == empty) {
                    availableMoves.add(new int[]{i, j});
                }
            }
        }
        return availableMoves;
    }

}
