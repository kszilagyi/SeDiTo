package com.kristofszilagyi.sedito.aligner;

import smile.feature.Scaler;

@SuppressWarnings("UnusedReturnValue") //this is just intellj not recognising scala
public class AccessibleScaler extends Scaler {
    public AccessibleScaler(boolean copy) {
        super(copy);
    }
    public double[] getLo() { return lo; }
    public double[] getHi() { return hi; }
}
