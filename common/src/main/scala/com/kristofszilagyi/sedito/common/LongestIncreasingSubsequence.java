package com.kristofszilagyi.sedito.common;

// Java program to find length of longest increasing subsequence
// in O(n Log n) time
// adapted from: https://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n/
//and https://www.geeksforgeeks.org/?p=27614
import java.util.*;

final public class LongestIncreasingSubsequence {

    // Binary search
    private static int getCeilIndex(int input[], int[] tailIndeices, int l, int r,
                     int key) {
        while (r - l > 1) {
            int m = l + (r - l) / 2;
            if (input[tailIndeices[m]] >= key)
                r = m;
            else
                l = m;
        }

        return r;
    }

    public static List<Integer> apply(int input[]) {
        // Add boundary case, when array n is zero
        // Depend on smart pointers

        int[] tailIndices = new int[input.length];
        int[] prevIndices = new int[input.length];
        Arrays.fill(prevIndices, -1);

        int len = 1; // it will always point to empty location
        for (int i = 1; i < input.length; i++) {
            if (input[i] < input[tailIndices[0]]) {
                // new smallest value
                tailIndices[0] = i;
            } else if (input[i] > input[tailIndices[len - 1]]) {
                // input[i] wants to extend largest subsequence
                prevIndices[i] = tailIndices[len - 1];
                tailIndices[len++] = i;
            } else {
                // input[i] wants to be a potential condidate of
                // future subsequence
                // It will replace ceil value in tailIndices
                int pos = getCeilIndex(input, tailIndices, -1,
                        len - 1, input[i]);

                prevIndices[i] = tailIndices[pos - 1];
                tailIndices[pos] = i;
            }
        }

        ArrayList<Integer> result = new ArrayList<>(len);
        for (int i = tailIndices[len-1]; i >= 0; i = prevIndices[i]) {
            result.add(input[i]);
        }
        Collections.reverse(result);
        return result;
    }
}
