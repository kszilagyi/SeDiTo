package com.kristofszilagyi.sedito.aligner;

// https://www.geeksforgeeks.org/print-longest-common-substring/
public class LongestCommonSubstring {

    /* function to find the longest common
       substring of X[0..m-1] and Y[0..n-1] */
    static <T> LongestCommonSubstringResult apply(T[] X, T[] Y)
    {
        int m = X.length;
        int n = Y.length;
        // Create a table to store lengths of longest common 
        // suffixes of substrings.   Note that LCSuff[i][j] 
        // contains length of longest common suffix of X[0..i-1] 
        // and Y[0..j-1]. The first row and first column entries 
        // have no logical meaning, they are used only for 
        // simplicity of program 
        int[][] LCSuff = new int[m + 1][n + 1];

        // To store length of the longest common substring 
        int maxLength = 0;

        // To store the index of the cell which contains the 
        // maximum value. This cell's index helps in building 
        // up the longest common substring from right to left. 
        int maxRow = 0, maxCol = 0;
  
        /* Following steps build LCSuff[m+1][n+1] in bottom 
           up fashion. */
        for (int i = 0; i <= m; i++) {
            for (int j = 0; j <= n; j++) {
                if (i == 0 || j == 0)
                    LCSuff[i][j] = 0;

                else if (X[i - 1] == Y[j - 1]) {
                    LCSuff[i][j] = LCSuff[i - 1][j - 1] + 1;
                    if (maxLength < LCSuff[i][j]) {
                        maxLength = LCSuff[i][j];
                        maxRow = i;
                        maxCol = j;
                    }
                }
                else
                    LCSuff[i][j] = 0;
            }
        }

        // if true, then no common substring exists 
        if (maxLength == 0) {
            return new LongestCommonSubstringResult(0, 0, 0);
        }
        return new LongestCommonSubstringResult(maxRow - maxLength, maxCol - maxLength, maxLength);
    }
}
// This code is contributed by Sumit Ghosh